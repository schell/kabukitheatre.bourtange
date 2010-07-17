float x,y;
float radius = 0.5f;
	glBegin(GL_LINES);
		glColor3f(0.0f,0.0f,0.0f);
		
		x = (float)radius * cos(359 * PI/180.0f);
		y = (float)radius * sin(359 * PI/180.0f);
		for(int j = 0; j < 360; j++)
		{
			glVertex2f(x,y);
			x = (float)radius * cos(j * PI/180.0f);
			y = (float)radius * sin(j * PI/180.0f);
			glVertex2f(x,y);
		}
	glEnd();