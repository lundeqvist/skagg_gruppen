package games.tictactoeHost;

import com.ericsson.otp.erlang.OtpMbox;
import utils.*;
import communication.*;
import javax.swing.*;

public class GameControl implements Runnable {

    private TicTacToeHost game;
    private int gameRows, gameCols, counter;
    private String gameID, hostID;
    private JButton[][] gameGrid;
    private TttPlayer x, o;
    private OtpMbox mailbox;
    private CommunicationWithErlang converter;

    public GameControl(TicTacToeHost TTTGame, TttPlayer x, TttPlayer o, String gameID) {
        hostID = gameID + "host";
        this.gameID = gameID;
        converter = new CommunicationWithErlang(x.getPlayerID());
        mailbox = converter.createMailbox(hostID, x.getPlayerID());
        Utils.sendMessage(mailbox, converter, hostID, x.getPlayerID(), "{join_game}");
        counter = 0;
        this.x = x;
        this.o = o;
        game = TTTGame;
        gameRows = game.getRows();
        gameCols = game.getCols();
        gameGrid = game.getGrid();
    }

    public void run() {
        Thread listener = new Thread(new ServerListener());
        listener.start();
    }

    public String getGameID() {
        return this.gameID;
    }

    public TttPlayer getPlayerX() {
        return this.x;
    }

    public TttPlayer getPlayerO() {
        return this.o;
    }

    public TicTacToeHost getTTT() {
        return game;
    }

    private class ServerListener implements Runnable {

        public ServerListener() {}

        private void serverListener() {
            while (true) {
                Arguments arguments = Utils.receiveMessage(mailbox, converter);
                String position = arguments.getArguments()[0];
                String playerType = (x.getPlayerID().equals(arguments.getPlayerID()) ? "X" : "O");
                int[] xy = Utils.splitCoordinates(position);
                if (!(((counter % 2 == 0) && playerType.equals("O"))
                        || ((counter % 2 != 0) && playerType.equals("X")))) {
                    gameGrid[xy[0]][xy[1]].setText(playerType);
                    Utils.sendMessage(mailbox, converter, gameID, arguments.getPlayerID(), "{" + xy[0] + xy[1] + ", " + winCheck(playerType) + "}");
                    counter++;
                }
            }
        }

        @Override
        public void run() {
            serverListener();
        }
    }

    private String winCheck(String type) {

        if (checkRows(type).equals("1") || checkCols(type).equals("1") || checkDiags(type).equals("1")) {
            return (type.equals("X") ? x.getPlayerID() : o.getPlayerID());
        } else {
            return boardFull();
        }
    }

    private String checkRows(String type) {
        int consecutive = 0;
        for (int i = 0; i < gameRows; i++) {
            for (int j = 0; j < gameCols; j++) {
                if (gameGrid[i][j].getText().equals(type)) {
                    consecutive += 1;
                    if (consecutive == 3) {
                        return "1";
                    }
                }
            }
            consecutive = 0;
        }
        return "0";
    }

    private String checkCols(String type) {
        int consecutive = 0;
        for (int i = 0; i < gameCols; i++) {
            for (int j = 0; j < gameRows; j++) {
                if (gameGrid[j][i].getText().equals(type)) {
                    consecutive += 1;
                    if (consecutive == 3) {
                        return "1";
                    }
                }
            }
            consecutive = 0;
        }
        return "0";

    }

    private String checkDiags(String type) {
        int consecutive = 0;
        for (int i = 0; i < gameRows; i++) {
            if (gameGrid[i][i].getText().equals(type)) {
                consecutive += 1;
                if (consecutive == 3) {
                    return "1";
                }
            }
        }
        consecutive = 0;
        for (int i = 0; i < gameRows; i++) {
            if (gameGrid[i][gameRows - 1 - i].getText().equals(type)) {
                consecutive += 1;
                if (consecutive == 3) {
                    return "1";
                }
            }
        }
        return "0";
    }

    public String boardFull() {
        for (int i = 0; i < gameRows; i++) {
            for (int j = 0; j < gameCols; j++) {
                if (!(gameGrid[i][j].getText().equals(x.getType()) || gameGrid[i][j].getText().equals(o.getType()))) {
                    System.out.println(gameGrid[i][j].getText());
                    return "0";
                }
            }
        }
        return "-1";
    }
}