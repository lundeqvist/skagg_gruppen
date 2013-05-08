package utils;

public class Arguments {

    private String gameID;
    private String playerID;
    private String[] arguments;
    
    public Arguments(String gameID, String playerID, String arguments) {
        this.gameID = gameID;
        this.playerID = playerID;
        this.arguments = Utils.parseTupleString(arguments);
    }
    
    public String getGameID() {
        return this.gameID;
    }

    public String getPlayerID() {
        return this.playerID;
    }
    
    public String[] getArguments(){
        return this.arguments;
    }
    
    public String argumentsToTuple(){
        return "{" + arguments.toString() + "}";
    }

}