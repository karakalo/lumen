// precision mediump float;

uniform sampler2D   sTexture; 

varying vec4        vColor;
varying vec2        vCoords;



void main()
{
   gl_FragColor = texture2D (sTexture, vCoords)  *  vColor;   // Modulate color with texture.
}

