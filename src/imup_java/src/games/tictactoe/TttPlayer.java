package games.tictactoe;

import player.Player;

public class TttPlayer extends Player{

	private final String type;
	
	public TttPlayer(String name, String type)
	{
		super(name);
		this.type = type;
	}
	
	public String getType()
	{
		return this.type;
	}
}
