package utils;

import com.ericsson.otp.erlang.*;
import communication.*;

/**
 *  Utils contains helpful functions that is needed by all classes.
 */
public class Utils {
    
    /**
     * Converts java arguments to erlang code, into a OtpErlangTuple so that it can be sent over
     * network.
     * 
     * @param mailbox the mailbox the arguments is sent from.
     * @param gameID 
     * @param playerID
     * @param arguments a string with as many arguments as you want represented
     *                  as a erlang tuple.
     * @return a OtPErlangTuple of all arguments as OtpErlangAtoms.
     */
    public static OtpErlangTuple convertToErlang(OtpMbox mailbox, String gameID, String playerID, String arguments) {
        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = mailbox.self();
        msg[1] = new OtpErlangAtom(gameID);
        msg[2] = new OtpErlangAtom(playerID);
        
        String[] parsedTuple = parseTupleString(arguments);
        OtpErlangObject[] args = new OtpErlangObject[parsedTuple.length];
        for(int i=0;i<parsedTuple.length;i++) {
            args[i] = new OtpErlangAtom(parsedTuple[i]);
        }
        msg[3] = new OtpErlangTuple(args);
        return new OtpErlangTuple(msg);
    }
    
    
    public static OtpErlangTuple convertUsersToErlang(OtpMbox mailbox, String gameID, String playerID, String arguments, String[] users) {
        OtpErlangObject[] msg = new OtpErlangObject[5];
        msg[0] = mailbox.self();
        msg[1] = new OtpErlangAtom(gameID);
        msg[2] = new OtpErlangAtom(playerID);
        
        String[] parsedTuple = parseTupleString(arguments);
        OtpErlangObject[] args = new OtpErlangObject[parsedTuple.length];
        for(int i=0;i<parsedTuple.length;i++) {
            args[i] = new OtpErlangAtom(parsedTuple[i]);
        }
        msg[3] = new OtpErlangTuple(args);
        
        OtpErlangObject[] usrs = new OtpErlangObject[users.length];
        for(int i=0;i<users.length;i++) {
            usrs[i] = new OtpErlangAtom(users[i]);
        }
        msg[4] = new OtpErlangList(usrs);
        return new OtpErlangTuple(msg);
    }    
    
    
    /**
     * Converts erlang code coming from server to java code and separates 
     * all the arguments to a Arguments.
     * @param robj OtpErlangObject
     * @return a Arguments object with all arguments.
     */
    public static Arguments convertToJava(OtpErlangObject robj) {
        OtpErlangTuple rtuple = (OtpErlangTuple) robj;
        String GameID = rtuple.elementAt(1).toString();
        String PlayerID = rtuple.elementAt(2).toString();
        String ArgumentsString = rtuple.elementAt(3).toString();
        return new Arguments(GameID, PlayerID, ArgumentsString);
    }
    
    public static Arguments convertUsersToJava(OtpErlangObject robj) {
        OtpErlangTuple rtuple = (OtpErlangTuple) robj;
        String GameID = rtuple.elementAt(1).toString();
        String PlayerID = rtuple.elementAt(2).toString();
        String ArgumentsString = rtuple.elementAt(3).toString();
        String UsersString = rtuple.elementAt(4).toString();
        return new Arguments(GameID, PlayerID, ArgumentsString, UsersString);
    }
    
    
    /**
     * Receives a message from erlang server
     * @param mailbox the mailbox the message should be received to.
     * @param converter 
     * @return a Arguments object with all arguments.
     */
    public static Arguments receiveMessage(OtpMbox mailbox, CommunicationWithErlang converter) {
        return convertToJava(converter.receive(mailbox));
    }
    
    public static Arguments receiveMessageUsers(OtpMbox mailbox, CommunicationWithErlang converter) {
        return convertUsersToJava(converter.receive(mailbox));
    }
    
    /**
     * Sends a message to a erlang server
     * @param mailbox
     * @param converter
     * @param gameID
     * @param playerID
     * @param arguments 
     */
    public static void sendMessage(OtpMbox mailbox, CommunicationWithErlang converter, String gameID, String playerID, String arguments) {
        converter.send(convertToErlang(mailbox, gameID, playerID, arguments), mailbox);
    }
    
    public static void sendMessageToUsers(OtpMbox mailbox, CommunicationWithErlang converter, String gameID, String playerID, String argument, String[] users) {
        converter.send(convertUsersToErlang(mailbox, gameID, playerID, argument, users), mailbox);
    }    
    
    public static int serverConnect() {
        return 0;
    }

    public static String[] tupleToStringArray(OtpErlangTuple tuple) {
        String[] tupleArgs = new String[tuple.arity()];
        for (int i = 0; i < tuple.arity(); i++) {
            tupleArgs[i] = tuple.elementAt(i).toString();
        }
        return tupleArgs;
    }

    public static String[] tupleToStringArray(OtpErlangTuple tuple, int length) {
        String[] tupleArgs = new String[length];
        for (int i = 0; i < length; i++) {
            if (tuple.elementAt(i) instanceof OtpErlangTuple) {
                String[] innerTuple = tupleToStringArray((OtpErlangTuple) tuple.elementAt(i), ((OtpErlangTuple) tuple.elementAt(i)).arity());
                for (int j = i; j < i + innerTuple.length; j++) {
                    tupleArgs[j] = innerTuple[j - i];
                }
                i += innerTuple.length;
            } else {
                tupleArgs[i] = tuple.elementAt(i).toString();
            }
        }
        return tupleArgs;
    }

    public static String[] parseTupleString(String tuple) {
        tuple = tuple.trim();
        tuple = tuple.substring(1, tuple.length() - 1);
        String[] parsedTuple = tuple.split(",");
        for (int i = 0; i < parsedTuple.length; i++) {
            parsedTuple[i] = parsedTuple[i].trim();
        }
        return parsedTuple;
    }
    
    public static int[] splitCoordinates(String coordinates) {
        coordinates = coordinates.replace("'","");
        int[] xy = new int[2];
        xy[0] = Integer.parseInt(coordinates.substring(0, 1));
        xy[1] = Integer.parseInt(coordinates.substring(1, 2));
        return xy;
    }
}
