//
//  Shader.fsh
//  OpenGLSample
//
//  Created by Sylvain Ageneau on 11/14/12.
//  Copyright (c) 2012 Self. All rights reserved.
//

varying lowp vec4 colorVarying;

void main()
{
    gl_FragColor = colorVarying;
}
