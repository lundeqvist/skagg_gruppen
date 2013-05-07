package player;

public abstract class Player {

	protected String playerID;
	
	public Player(String playerID)
	{
		this.playerID = playerID;
	}
	
	public String getPlayerID()
	{
		return this.playerID;
	}
	
	public void setPlayerID(String playerID)
	{
		this.playerID = playerID;
	}
}
