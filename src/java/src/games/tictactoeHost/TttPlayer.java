package games.tictactoeHost;

import player.Player;

public class TttPlayer extends Player{

	private final String type;
	
	public TttPlayer(String playerID, String type)
	{
		super(playerID);
		this.type = type;
	}
	
	public String getType()
	{
		return this.type;
	}
}
