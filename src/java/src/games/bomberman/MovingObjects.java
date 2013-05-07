package games.bomberman;

import javax.swing.*;
import javax.swing.Timer;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Ellipse2D;
import java.awt.Point;

@SuppressWarnings("serial")
public class MovingObjects extends JPanel {

	private Timer t;
	double x = 0, y = 0, velX, velY, sizeX, sizeY;
	ImmaBomb[] bombs = new ImmaBomb[10];
	int bombindex = 0;
	Image rabbit;
	
	public MovingObjects()
	{
		init_panel();
	}
	
	private void init_panel()
	{
		this.setBorder(getBorder());
		t = new Timer(1,new SettingsListener());
		t.start();
		addKeyListener(new ScreenListener());
		addMouseListener(new ClickListener());
		setFocusable(true);
		setFocusTraversalKeysEnabled(false);

	}
	
	public void paintComponent(Graphics g)
	{
		super.paintComponent(g);
		Graphics2D g2 = (Graphics2D) g;
		g2.fill(new Ellipse2D.Double(x,y,40,40));
		for(int i = 0; i<bombindex; i++)
		{
			g2.fill(bombs[i].getGraphics());
		}
	}
	
	public void up()
	{
		velY = -0.8;
		velX = 0;
	}
	
	public void down()
	{
		velY = 0.8;
		velX = 0;
	}
	
	public void left()
	{
		velY = 0;
		velX = -0.8;
	}
	
	public void right()
	{
		velY = 0;
		velX = 0.8;
	}
	
	public void drop()
	{
		bombs[bombindex] = new ImmaBomb(x,y);
		if(bombindex<9)
			bombindex++;
	}
	
	
	
	
	private class SettingsListener implements ActionListener
	{

		@Override
		public void actionPerformed(ActionEvent e) {
			repaint();
			if(x<0 || x>545)
				velX = -velX;
			if(y<0 || y>523)
				velY = -velY;
			x += velX;
			y += velY;
		}
		
	}
	
	private class ScreenListener implements KeyListener
	{

		@Override
		public void keyPressed(KeyEvent e) 
		{
			switch(e.getKeyCode())
			{
				case KeyEvent.VK_UP:
					up();
					break;
				case KeyEvent.VK_DOWN:
					down();
					break;
				case KeyEvent.VK_LEFT:
					left();
					break;
				case KeyEvent.VK_RIGHT:
					right();
					break;
				case KeyEvent.VK_SPACE:
					drop();
					break;
				default:
					velX = 0;
					velY = 0;
					break;
			}
			
		}

		@Override
		public void keyReleased(KeyEvent e) {
			switch(e.getKeyCode())
			{
				default:
					velX = 0;
					velY = 0;
					break;
			}
			
		}

		@Override
		public void keyTyped(KeyEvent arg0) {
			// TODO Auto-generated method stub
			
		}
	}
	private class ClickListener implements MouseListener
	{

		@Override
		public void mouseClicked(MouseEvent e) {
			
	
		}

		@Override
		public void mouseEntered(MouseEvent arg0) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void mouseExited(MouseEvent arg0) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void mousePressed(MouseEvent e) {
			
			Point p = e.getPoint();
			double px = p.getX();
			double py = p.getY();
			
			double xalign = -(x-px);
			double yalign = -(y-py);
			double lol = xalign/0.3;
			double loly = xalign/0.3;
			if(lol<0)
				lol = -lol;
			velX = xalign/lol;
			velY = yalign/lol;
			
		}

		@Override
		public void mouseReleased(MouseEvent arg0) {
			// TODO Auto-generated method stub
			
		}
		
	}
}
