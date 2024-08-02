module Main exposing (..)

import Browser
import Random
import Time
import Task
import WebGL

import Browser.Dom exposing (Viewport, getViewportOf)
import Browser.Events exposing (onResize, onAnimationFrameDelta)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import Html exposing (..)
import Html.Attributes exposing (..)

type alias Flags = {}

type alias Model =
  { deltaTime : Float
  , webGLViewport : Maybe Viewport 
  , particles : List Particle
  }

type Msg 
  = WindowSize Int Int
  | WebGLViewport (Result Browser.Dom.Error Viewport)
  | Frame Float
  | Tick 
  | Particles (List Particle)

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : Flags -> ( Model, Cmd Msg )
init _ = 
  ( { deltaTime = 0
    , webGLViewport = Nothing 
    , particles = []
    }
  , Task.attempt WebGLViewport (getViewportOf "webgl") 
  )

view : Model -> Html Msg
view model = 
  section
    [ class """w-screen h-screen bg-black 
               text-white flex flex-row overflow-hidden"""
    ]
    [ div [ class "w-1/2 p-10 text-3xl font-mono" ]
      [ text "GG Data and Systems Co."
      ]
    , canvasHtml model
        |> div [ id "webgl", class "w-1/2 bg-white" ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    WindowSize _ _ -> ( model, Task.attempt WebGLViewport (getViewportOf "webgl") )
    WebGLViewport result -> 
      case result of
        Ok viewport -> ( { model | webGLViewport = Just viewport }, Cmd.none )
        Err e -> ( { model | webGLViewport = Nothing }, Cmd.none )
    Frame deltaTime -> 
      ( { model 
        | deltaTime = deltaTime
        , particles = stepParticles deltaTime model.particles 
        }
      , Cmd.none 
      )
    Tick ->
      case model.webGLViewport of
        Nothing -> ( model, Cmd.none ) 
        Just viewport -> 
          ( model
          , randomParticles 2 (vec2 viewport.viewport.width viewport.viewport.height) Particles 
          )
    Particles particles -> 
      ( { model | particles = List.append model.particles particles }
      , Cmd.none 
      )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ onResize WindowSize
    , onAnimationFrameDelta Frame
    , Time.every 500 (\_ -> Tick)
    ]

type alias Vertex =
  { position : Vec2
  , color : Vec3 
  }

type alias Uniforms = 
  { projection : Mat4 
  }

vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader = 
  [glsl| 
    uniform mat4 projection;
    attribute vec2 position;
    attribute vec3 color; 
    varying vec3 vcolor;

    void main() {
      gl_Position = projection * vec4(position, 0.0, 1.0);
      vcolor = color;
    }
  |]

fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
  [glsl| 
    precision mediump float;
    varying vec3 vcolor;

    void main() {
      gl_FragColor = vec4(vcolor, 1.0);
    }
  |]

type alias Particle = 
  { life : Float
  , position : Vec2 
  , velocity : Vec2
  }

maxParticleLife : Float
maxParticleLife = 5000

maxParticleRange : Float
maxParticleRange = 200

particleDampening : Float
particleDampening = 0.99

particleColor : Particle -> Vec3
particleColor particle = 
  Vec3.scale (particle.life / maxParticleLife) (vec3 1 1 1)

stepParticle : Float -> Particle -> Particle
stepParticle deltaTime particle = 
  { life = particle.life + deltaTime
  , position = Vec2.add particle.position (Vec2.scale deltaTime particle.velocity)
  , velocity = Vec2.scale particleDampening particle.velocity
  }

stepParticles : Float -> List Particle -> List Particle
stepParticles deltaTime particles =
  particles 
    |> List.map (stepParticle deltaTime)
    |> List.filter (\p -> p.life < maxParticleLife)

nearbyParticles : Vec2 -> List Particle -> List Particle
nearbyParticles position particles = 
  List.filter (\p -> Vec2.distance p.position position < maxParticleRange) particles

particleGroups : List Particle -> List ( Particle, List Particle )
particleGroups particles =
  List.filterMap 
    (\p -> 
      case nearbyParticles p.position particles of 
        [] -> Nothing
        xs -> Just ( p, xs ) 
    )
    particles

buildParticleMesh : List Particle -> WebGL.Mesh Vertex
buildParticleMesh particles = 
  particles
    |> particleGroups
    |> List.concatMap 
        (\( v, ws ) -> 
          List.map 
            (\w -> 
              ( Vertex v.position (particleColor v)
              , Vertex w.position (particleColor w) 
              )
            )
          ws
        )
    |> WebGL.lines

randomParticles : Int -> Vec2 -> (List Particle -> msg) -> Cmd msg
randomParticles count bounds f = 
  let 
    mkParticle ( x, ( dir, speed ) ) = 
      { life = 0
      , position = vec2 x (50 + (Vec2.getY bounds))
      , velocity = Vec2.scale speed (vec2 -(Basics.cos dir) -(Basics.sin dir))
      }
  in
    Random.pair 
        (Random.float 0 (Vec2.getX bounds))
        (Random.pair 
          (Random.float (Basics.pi / 6) (5 * Basics.pi / 6)) 
          (Random.float 0.075 0.5))
      |> Random.map mkParticle
      |> Random.list count
      |> Random.generate f

canvasHtml : Model -> List (Html Msg) 
canvasHtml model = 
  case model.webGLViewport of
    Nothing -> []
    Just viewport -> 
      [ WebGL.toHtmlWith
        [ WebGL.antialias
        , WebGL.clearColor 1 1 1 1 
        ]
        [ viewport.viewport.width 
            |> ceiling
            |> width
        , viewport.viewport.height
            |> ceiling
            |> height 
        ]
        [ WebGL.entity 
            vertexShader 
            fragmentShader
            (buildParticleMesh model.particles)
            { projection = Mat4.makeOrtho2D 0 viewport.viewport.width viewport.viewport.height 0 }
        ]
      ]