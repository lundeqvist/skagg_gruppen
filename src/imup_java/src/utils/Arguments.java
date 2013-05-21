package utils;

public class Arguments {

    private String gameID;
    private String playerID;
    private String[] arguments, users;
    
    public Arguments(String gameID, String playerID, String arguments) {
        this.gameID = gameID;
        this.playerID = playerID;
        this.arguments = Utils.parseTupleString(arguments);
    }
    
    public Arguments(String gameID, String playerID, String arguments, String users) {
        this(gameID,playerID,arguments);
        this.users = Utils.parseTupleString(users);
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
    
    public String[] getUsers(){
        return this.users;
    }
    
    public String argumentsToTuple(){
        return "{" + arguments.toString() + "}";
    }

}