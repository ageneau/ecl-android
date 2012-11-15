;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;; shader-vao.lisp --- Example usage of vertex and fragment shaders,
;;; vertex buffer objects, and vertex array objects
(in-package #:openglsample)

(defclass shader-vao-window ()
  ((vbuff :accessor vertex-buffer)
   (ibuff :accessor index-buffer)
   (vs :accessor vertex-shader)
   (fs :accessor fragment-shader)
   (va :accessor vertex-array)
   (program :accessor program)
   (position-slot :accessor position-slot)
   (color-slot :accessor color-slot)))

(defvar *vertex-data*
  #(
    ;; Position Color 
    1 -1 0 	1 0 0 1
    1 1 0 	0 1 0 1
    -1 1 0 	0 0 1 1
    -1 -1 0 	0 0 0 1
    ))

(defvar *shader-vao-vertex-program*
  "
attribute vec4 Position; // 1
attribute vec4 SourceColor; // 2
 
varying vec4 DestinationColor; // 3
 
void main(void) { // 4
    DestinationColor = SourceColor; // 5
    gl_Position = Position; // 6
} 
")

(defvar *shader-vao-fragment-program*
  "
varying lowp vec4 DestinationColor; // 1
 
void main(void) { // 2
    gl_FragColor = DestinationColor; // 3
}
")

;;; Initialization 

;;; First, we create buffers for our vertex and index
;;; data. Then, we create the vertex array object that we actually use
;;; for rendering directly. Finally, we load the shader objects.
(defmethod setup ((w shader-vao-window))
  ;; An array buffer can be used to store verex position, colors,
  ;; normals, or other data. We need to allocate an GL array, copy the
  ;; data to the array, and tell OpenGL that the buffers data comes
  ;; from this GL array. Like most OpenGL state objects, we bind the
  ;; buffer before we can make changes to its state.
  (unless (gl::features-present-p (>= :glsl-version 1.0))
    (error "Need GLSL 1.0")
    (return-from setup nil))
  (let ((buffers (gl:gen-buffers 2)))
    (setf (vertex-buffer w) (elt buffers 0)
	  (index-buffer w) (elt buffers 1)))
  (gl:bind-buffer :array-buffer (vertex-buffer w))
  (let ((arr (gl:alloc-gl-array :float 28))
	(verts *vertex-data*))
    (dotimes (i (length verts))
      (setf (gl:glaref arr i) (aref verts i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))

  ;; 0 is always reserved as an unbound object.
  (gl:bind-buffer :array-buffer 0)

  ;; An element array buffer stores vertex indices. We fill it in the
  ;; same way as an array buffer.
  (gl:bind-buffer :element-array-buffer (index-buffer w))
  (let ((arr (gl:alloc-gl-array :unsigned-short 6))
	(indexes #(0 1 2
                   2 3 0)))
    (dotimes (i (length indexes))
      (setf (gl:glaref arr i) (aref indexes i)))
    (gl:buffer-data :element-array-buffer :static-draw arr)
    (gl:free-gl-array arr))
  (gl:bind-buffer :element-array-buffer 0)

  ;; A program object is a collection of shader objects to be used
  ;; together in a single pipeline for rendering objects. To create a
  ;; program, you first create the individual shaders. Then you attach
  ;; the shaders to the program and link the program together.
  (let ((vs (gl:create-shader :vertex-shader))
	(fs (gl:create-shader :fragment-shader)))
    (setf (vertex-shader w) vs)
    (setf (fragment-shader w) fs)
    (gl:shader-source vs *shader-vao-vertex-program*)
    (gl:compile-shader vs)
    (gl:shader-source fs *shader-vao-fragment-program*)
    (gl:compile-shader fs)
    ;; If the shader doesn't compile, you can print errors with:
    (format t "Vertex shader info log: ~a~%END~%" (gl:get-shader-info-log vs))
    (format t "Fragment shader info log: ~a~%END~%" (gl:get-shader-info-log fs))

    (setf (program w) (gl:create-program))
    
    ;; You can attach the same shader to multiple different programs.
    (gl:attach-shader (program w) vs)
    (gl:attach-shader (program w) fs)

    ;; Don't forget to link the program after attaching the
    ;; shaders. This step actually puts the attached shader together
    ;; to form the program.
    (gl:link-program (program w))
    ;; If we want to render using this program object, or add
    ;; uniforms, we need to use the program. This is similar to
    ;; binding a buffer.
    (gl:use-program (program w))

    ;; Vertex array objects manage which vertex attributes are
    ;; associated with which data buffers. 
    (setf (vertex-array w) (gl:gen-vertex-array))
    (gl:bind-vertex-array-oes (vertex-array w))

    ;; To associate our VBO data with this VAO, we bind it, specify
    ;; which vertex attribute we want to associate it with, and specify
    ;; where the data comes from.
    (gl:bind-buffer :array-buffer (vertex-buffer w))
    
    (setf (position-slot w) (gl:get-attrib-location (program w) "Position"))
    (setf (color-slot w) (gl:get-attrib-location (program w) "SourceColor"))
    (gl:enable-vertex-attrib-array (position-slot w))

    ;; Using a null pointer as the data source indicates that we want
    ;; the vertex data to come from the currently bound array-buffer.
    (gl:vertex-attrib-pointer (position-slot w) 3 :float nil 28 (cffi:null-pointer))
    (gl:enable-vertex-attrib-array (color-slot w))
    (gl:vertex-attrib-pointer (color-slot w) 4 :float nil 28 (cffi:make-pointer 12))

    ;; To associate an element array with this VAO, all we need to do is
    ;; bind the element array buffer we want to use.
    (gl:bind-buffer :element-array-buffer (index-buffer w))

    ;; Once we're done, we can unbind the VAO, and rebind it when we want to render it.
    (gl:bind-vertex-array-oes 0)))

(defmethod display ((w shader-vao-window) width height)
  (gl:clear-color 0.0 0.4 0.22 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (gl:viewport 0 0 width height)
  
  ;; Since we never use any other program object, this is unnecessary
  ;; in this program. Typically, though, you'll have multiple program
  ;; objects, so you'll need to 'use' each one to activate it.
  (gl:use-program (program w))
  (gl:bind-vertex-array-oes (vertex-array w))

  
  ;; This call actually does the rendering. The vertex data comes from
  ;; the currently-bound VAO. If the input array is null, the indices
  ;; will be taken from the element array buffer bound in the current
  ;; VAO.
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 6))


;; Cleanup.
;; Most of the objects we created have analogous deletion function.
(defmethod cleanup ((w shader-vao-window))
  ;; Note: It doesn't matter whether we delete the program or the
  ;; linked shaders first. If a shader is linked to a program, the
  ;; shader isn't destroyed until after the program is
  ;; destroyed. Similarly, if the program is destroyed, the shaders
  ;; are detached.
  (when (slot-boundp w 'vs)
   (gl:delete-shader (vertex-shader w)))
  (when (slot-boundp w 'fs)
    (gl:delete-shader (fragment-shader w)))
  (when (slot-boundp w 'program)
   (gl:delete-program (program w)))

  (when (slot-boundp w 'vbuff)
    (gl:delete-buffers (list (vertex-buffer w) (index-buffer w))))
  (when (slot-boundp w 'va)
   (gl:delete-vertex-arrays (list (vertex-array w)))))

