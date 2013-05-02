package games.bomberman;

import java.awt.Point;
import java.awt.geom.Ellipse2D;

public class ImmaBomb {
	private Point coords;
	private long createTime, liveTime;
	
	public ImmaBomb(double x,  double y)
	{
		coords = new Point();
		coords.setLocation(x,y);
		createTime = System.currentTimeMillis();
	}
	
	public double coordX()
	{
		return coords.getX();
	}
	
	public double coordY()
	{
		return coords.getY();
	}
	
	public Ellipse2D getGraphics()
	{
		liveTime = System.currentTimeMillis() - createTime;
		
		if(3000<liveTime)
		{
			return new Ellipse2D.Double(coordX()-5,coordY()-5,50,50);
		}
		else
			return new Ellipse2D.Double(coordX()+15,coordY()+15,10,10);
	}
}
