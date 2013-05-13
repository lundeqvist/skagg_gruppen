package games.tictactoeHost;

import com.ericsson.otp.erlang.OtpMbox;
import utils.*;
import communication.*;
import javax.swing.*;

public class GameControl {

    private TicTacToe game;
    private int gameRows, gameCols, counter;
    private String gameID;
    private JButton[][] gameGrid;
    private TttPlayer x, o;
    private OtpMbox mailbox;
    private CommunicationWithErlang converter;  

    public GameControl(TicTacToe TTTGame, TttPlayer x, TttPlayer o) {
        converter = new CommunicationWithErlang();
        counter = 0;
        this.x = x;
        this.o = o;
        game = TTTGame;
        gameRows = game.getRows();
        gameCols = game.getCols();
        gameGrid = game.getGrid();
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
    
    public TicTacToe getTTT() {
        return game;
    }

    private void ServerListener() {
        while (true) {
            Arguments arguments = Utils.receiveMessage(mailbox, converter);
            String position = arguments.getArguments()[0];
            
            int[] xy = Utils.splitCoordinates(position);
                
                String playerType = (x.getPlayerID().equals(arguments.getPlayerID()) ? "X" : "O");
                if (!((counter % 2 == 0) && playerType.equals("O")) 
                        || ((counter % 2 != 0) && playerType.equals("X"))) {
                    Utils.sendMessage(mailbox, converter, getGameID(), "host", "{" + xy[0] + xy[1] + ", " + winCheck(playerType) + "}");
                    counter++;
                }
            }
    }
    
    
    public String winCheck(String type) {
        for (int i = 1; i < gameRows - 1; i++) {
            for (int j = 1; j < gameCols - 1; j++) {
                return checkSurroundings(i, j, type);
            }
        }
        return "0";
    }

    private String checkSurroundings(int i, int j, String t) {
        TttPlayer player = (t.equals("X") ? x : o);
        String type = player.getType();
        if (i == 1) {
            if (gameGrid[0][j - 1].getText().equals(type)
                    && gameGrid[0][j].getText().equals(type)
                    && gameGrid[0][j + 1].getText().equals(type)) {
                return player.getPlayerID();
            }
        }
        if (j == 1) {
            if (gameGrid[i - 1][0].getText().equals(type)
                    && gameGrid[i][0].getText().equals(type)
                    && gameGrid[i + 1][0].getText().equals(type)) {
                return player.getPlayerID();
            }
        }
        if (i == gameRows - 2) {
            if (gameGrid[gameRows - 1][j - 1].getText().equals(type)
                    && gameGrid[gameRows - 1][j].getText().equals(type)
                    && gameGrid[gameRows - 1][j + 1].getText().equals(type)) {
                return player.getPlayerID();
            }
        }
        if (j == gameCols - 2) {
            if (gameGrid[i - 1][gameCols - 1].getText().equals(type)
                    && gameGrid[i][gameCols - 1].getText().equals(type)
                    && gameGrid[i + 1][gameCols - 1].getText().equals(type)) {
                return player.getPlayerID();
            }
        }

        //rowcheck
        if (gameGrid[i][j - 1].getText().equals(type)
                && gameGrid[i][j].getText().equals(type)
                && gameGrid[i][j + 1].getText().equals(type)) {
                return player.getPlayerID();
        } //colcheck
        else if (gameGrid[i - 1][j].getText().equals(type)
                && gameGrid[i][j].getText().equals(type)
                && gameGrid[i + 1][j].getText().equals(type)) {
                return player.getPlayerID();
        } //right diagonalcheck
        else if (gameGrid[i - 1][j - 1].getText().equals(type)
                && gameGrid[i][j].getText().equals(type)
                && gameGrid[i + 1][j + 1].getText().equals(type)) {
                return player.getPlayerID();
        } else if (gameGrid[i - 1][j + 1].getText().equals(type)
                && gameGrid[i][j].getText().equals(type)
                && gameGrid[i + 1][j - 1].getText().equals(type)) {
                return player.getPlayerID();
        } else {
            return boardFull();
        }

    }

    public String boardFull() {
        for (int i = 0; i < gameRows; i++) {
            for (int j = 0; j < gameCols; j++) {
                if (!(gameGrid[i][j].getText().equals(x.getType()) || gameGrid[i][j].getText().equals(o.getType()))) {
                    return "0";
                }
            }
        }
        return "-1";
    }

}