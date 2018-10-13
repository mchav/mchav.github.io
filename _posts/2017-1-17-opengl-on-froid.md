---
layout: post
title: OpenGL on froid
---

The design and implementation of [froid](https://github.com/mchav/froid) continues. I am currently finishing up Criminal Intent from the BigNerd Ranch Android programming book. After work on Criminal Intent is done I will push the changes along with the application to a repository and that will mark version 0.0.2 of froid.

In the mean time, I worked on getting OpenGL to work on froid. After some tinkering I got up and working with an example application.

The example for this blog post will be a touch-rotated cube. I assume some knowledge of OpenGL.

[Quick demo](https://twitter.com/_mchav/status/821558137023066112)

I'll take a bottom up approach and start from the cube, then define the renderer, and finally the surfaceview. We first need to define a cube. Since in OpenGL, an shape is a collection of Buffers we define the Cube data type as:

```haskell
data Cube = CubeBuffers { vertexBuffer :: MutableIO IntBuffer
                        , colorBuffer  :: MutableIO IntBuffer
                        , indexBuffer  :: MutableIO ByteBuffer
                        }
```

The buffers are plain `java.nio` buffers. These are the buffers that will be given to OpenGL to render. We'll make a function called `newCube` constructs these buffers from some Frege lists. We'll draw the cube using `GL10.glTriangles` so we'll need an index buffer and therefore an index array. The lists for our buffers are therefore:

```haskell
one :: Int
one = 0x10000

vertices :: [Int]
vertices =  [ (-one), (-one), (-one)
            , one   , (-one), (-one)
            , one   ,  one  , (-one)
            , (-one),  one  , (-one)
            , (-one), (-one),  one
            , one   , (-one),  one
            , one   ,  one  ,  one
            , (-one),  one  ,  one
            ]

colors :: [Int]
colors =    [ 0  ,    0,    0,  one
            , one,    0,    0,  one
            , one,  one,    0,  one
            , 0  ,  one,    0,  one
            , 0  ,    0,  one,  one
            , one,    0,  one,  one
            , one,  one,  one,  one
            , 0  ,  one,  one,  one
            ]

indices :: [Byte]
indices =   [ 0, 4, 5,    0, 5, 1
            , 1, 5, 6,    1, 6, 2
            , 2, 6, 7,    2, 7, 3
            , 3, 7, 4,    3, 4, 0
            , 4, 7, 6,    4, 6, 5
            , 3, 0, 1,    3, 1, 2
            ]
```

The vertex and color buffers are IntBuffers and the index buffer is a byte buffer. For the IntBuffers we will start by making ByteBuffers, setting their order to nativeOrder, putting all the elements from the lists and then setting the buffer position to 0. We can abstract this task to a function `initIntBuffer`. We also define a function `initByteBuffer` that simply adds values to a buffer and initalises the position.

```haskell
initIntBuffer :: [Int] -> IOMutable IntBuffer
initIntBuffer xs = do
    nativeByteOrder <- ByteOrder.nativeOrder ()
    bb <- ByteBuffer.allocateDirect ((length xs) * 4)
    bb.order nativeByteOrder
    buffer <- bb.asIntBuffer
    bs <- arrayFromListST xs
    buffer.put bs
    buffer.position 0
    return buffer

initByteBuffer :: [Byte] -> IOMutable ByteBuffer
initByteBuffer xs = do
    bb <- ByteBuffer.allocateDirect (length xs)
    bytes <- arrayFromListST xs
    bb.put bytes
    bb.position 0
    return bb
```

So our cube constructor simply constructs the buffers and places them in our `Cube` data type. To draw the cube we need only set up the vertex and colour pointers then finally draw some triangles using the index buffer. This part will read like regular OpenGL

```haskell
newCube :: IO Cube
newCube = do
    vertexBuffer <- initIntBuffer vertices
    colorBuffer <- initIntBuffer colors
    indexBuffer <- initByteBuffer indices
    return CubeBuffers { vertexBuffer = vertexBuffer, colorBuffer = colorBuffer, indexBuffer = indexBuffer }

drawCube :: MutableIO GL10 -> Cube -> IO ()
drawCube gl cube = do
    gl.glFrontFace GL10.glCW
    gl.glVertexPointer 3 GL10.glFixed 0 cube.vertexBuffer
    gl.glColorPointer 4 GL10.glFixed 0 cube.colorBuffer
    gl.glDrawElements GL10.glTriangle 36 GL10.glUnsignedByte cube.indexBuffer
```

Now we can move on to our renderer. `froid` contains a renderer class called GLSurfaceViewRenderer. This approach, of having a dedicated renderer class is as powerful as having a renderer interface. It allows you to separate the rendering process from other kinds of effects that the program might have, or baking a renderer into a data type. `froid` "extends" objects by providing delegators which one can use to assign Frege methods to an object. Higher order functions seem to be powerful enough to model the kind of inheritance we care about. The delegator for the Renderer is the `GlsvRendererDelegator` data type which is a record of types `Maybe f` where f is a function.
Because we want our renderer to be able to draw a cube at various angles depnding on where/how we touch the screen we know it will take some mutable reference to those variables and include those in the closures of our delegated functions. The variables in the function signature are all passed in at runtime.

A renderer must implement 3 functions: `onDrawFrame`, `onSurfaceCreated`, and `onSurfaceChanged`. Let's go ahead and define that portion of the program.

```haskell
-- cube renderer
newCubeRenderer :: IORef Float -> IORef Float -> STMutable RealWorld GLSurfaceViewRenderer
newCubeRenderer angleX angleY = do
    cube <- newCube
    mkGLSurfaceViewRenderer (rendererDelegator cube angleX angleY)

rendererDelegator :: Cube -> IORef Float -> IORef Float -> GlsvRendererDelegator
rendererDelegator cube x y = GlsvRendererDelegator { onDrawFrame = Just (onDrawFrame cube x y)
                                                   , onSurfaceCreated = Just onSurfaceCreated
                                                   , onSurfaceChanged = Just onSurfaceChanged
                                                   }

onDrawFrame :: Cube -> IORef Float -> IORef Float -> MutableIO GL10 -> IO ()
onDrawFrame cube angleX angleY gl = do
    x <- readIORef angleX
    y <- readIORef angleY
    gl.glClear (GL10.glColorBufferBit .|. GL10.glDepthBufferBit)
    gl.glMatrixMode GL10.glModelView
    gl.glLoadIdentity
    gl.glTranslatef 0 0 (-3)
    gl.glRotatef x 0 1 0
    gl.glRotatef y 1 0 0
    gl.glEnableClientState GL10.glVertexArray
    gl.glEnableClientState GL10.glColorArray
    drawCube gl cube

onSurfaceCreated :: MutableIO GL10 ->  MutableIO EGLConfig -> IO ()
onSurfaceCreated gl config = do
    gl.glDisable GL10.glDither
    gl.glHint GL10.glPerspectiveCorrectHint GL10.glFastest
    gl.glClearColor 1 1 1 1
    gl.glEnable GL10.glCullFace
    gl.glShadeModel GL10.glSmooth
    gl.glEnable GL10.glDepthTest

onSurfaceChanged :: MutableIO GL10 -> Int -> Int -> IO ()
onSurfaceChanged gl width height = do
    gl.glViewport 0 0 width height
    let ratio = width.float / height.float
    gl.glMatrixMode GL10.glProjection
    gl.glLoadIdentity
    gl.glFrustumf (-ratio) ratio (-1) 1 1 10
```

Now let's define our surfaceview in a similar way. This time, instead, delegating to `onTouchEvent` and `onTrackball` event so the cube can respond to touch. We define 4 mutable variables - the x and y angles of the cube as well as their previous positions for calculations and define some scaling constants for each kind of device.

```haskell
-- GLSurfaceView

newTouchSurfaceView :: MutableIO Context -> IOMutable GLSurfaceView
newTouchSurfaceView context = do
    angleX <- newIORef (0 :: Float)
    angleY <-  newIORef (0 :: Float)
    previousX <- newIORef (0 :: Float)
    previousY <- newIORef (0 :: Float)
    renderer <- newCubeRenderer angleX angleY
    surfaceView <- mkGLSurfaceView context (surfaceViewDelegator angleX angleY previousX previousY)
    surfaceView.setRenderer renderer
    surfaceView.setRenderMode GLSurfaceView.renderModeWhenDirty
    return surfaceView

surfaceViewDelegator :: IORef Float -> IORef Float -> IORef Float -> IORef Float -> GLSurfaceViewDelegator
surfaceViewDelegator ax ay px py = defaultGLSurfaceView.{ onTouchEvent = Just (onTouchEvent px py ax ay)
                                                        , onTrackballEvent = Just (onTrackballEvent ax ay )
                                                        }


onTrackballEvent :: IORef Float -> IORef Float ->
                    MutableIO GLSurfaceView -> MutableIO MotionEvent -> IO Bool
onTrackballEvent angleX angleY this e = do
    x <- e.getX
    y <- e.getY
    modifyIORef angleX (+ (x * trackballScaleFactor))
    modifyIORef angleY (+ (y * trackballScaleFactor))
    this.requestRender
    return True

onTouchEvent :: IORef Float -> IORef Float -> IORef Float -> IORef Float ->
                MutableIO GLSurfaceView -> MutableIO MotionEvent -> IO Bool
onTouchEvent previousX previousY angleX angleY this e = do
    touchAction <- e.getActionMasked
    x <- e.getX
    y <- e.getY
    prevX <- readIORef previousX
    prevY <- readIORef previousY
    -- update angles
    let (dx, dy) = updateAngles (x - prevX) (y - prevY)
    zipWithM_ writeIORef [angleX, angleY, previousX, previousY] [x, y, dx, dy]
    this.requestRender
    return True

updateAngles :: Float -> Float -> (Float, Float)
updateAngles dx dy = ((dx * touchScaleFactor), (dy * touchScaleFactor))

touchScaleFactor :: Float
touchScaleFactor = 180.0 / 320

trackballScaleFactor :: Float
trackballScaleFactor = 36.0
```

Finally we link this all to an activity. The important thing to remember is that we call `GLSurfaceView`'s onPause and onResume when we call the Activities's onPause and onResume so it can reload its state.

```haskell
module io.github.mchav.touchcube.CubeActivity where

import froid.javax.microedition.khronos.egl.EGLConfig
import froid.javax.microedition.khronos.opengles.GL10

import froid.java.nio.ByteBuffer
import froid.java.nio.IntBuffer

import froid.app.Activity
import froid.content.Context
import froid.opengl.GLSurfaceView
import froid.opengl.glSurfaceView.java.NativeRenderer
import froid.opengl.glSurfaceView.Renderer
import froid.os.Bundle
import froid.view.InputDevice
import froid.view.MotionEvent
import froid.view.View

import Data.Bits

native module type Activity where {}

onCreate :: MutableIO Activity -> Maybe (MutableIO Bundle) -> IO ()
onCreate this bundle = do
    context <- this.getApplicationContext
    glSurfaceView <- newTouchSurfaceView context
    view <- glSurfaceView.asView
    this.setContentView (view :: MutableIO View)
    glSurfaceView.requestFocus
    glSurfaceView.setFocusableInTouchMode True
    this.setOnResume glSurfaceView.onResume
    this.setOnPause glSurfaceView.onPause
```

[This project is available on github gist](https://gist.github.com/mchav/5460506b60f54d61788dfe534f61a16f)
