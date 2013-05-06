package games.tictactoeHost;

import javax.swing.*;

public class GameControl {

    private TicTacToe game;
    private int gameRows, gameCols, counter;
    private String gameID;
    private JButton[][] gameGrid;
    private TttPlayer x, o;

    public GameControl(String playerX, String playerO) {
        counter = 0;
        x = new TttPlayer(playerX, "X");
        o = new TttPlayer(playerO, "O");
        game = new TicTacToe(3, 3, this);
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

    private class ServerListener {
        // HÄMTA FÖRFRÅGAN FRÅN SERVER
        public void serverPerformed() {
            while (true) {
                {gameID, playerID, newPosition} = receive_MoveReq();
                {i,j} = newPosition;        
                
                String playerType = (x.getName().equals(playerID) ? "X" : "O");
                if (((counter % 2 == 0) && playerType.equals("O")) 
                        || ((counter % 2 != 0) && playerType.equals("X"))) {}
             
                // SKICKA setText till server
                // send_Move(i+j, get_playerID(), get_gameID());
                // send_cmd(winCheck(playerType), get_playerID(), get_gameID());
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
                return player.getName();
            }
        }
        if (j == 1) {
            if (gameGrid[i - 1][0].getText().equals(type)
                    && gameGrid[i][0].getText().equals(type)
                    && gameGrid[i + 1][0].getText().equals(type)) {
                return player.getName();
            }
        }
        if (i == gameRows - 2) {
            if (gameGrid[gameRows - 1][j - 1].getText().equals(type)
                    && gameGrid[gameRows - 1][j].getText().equals(type)
                    && gameGrid[gameRows - 1][j + 1].getText().equals(type)) {
                return player.getName();
            }
        }
        if (j == gameCols - 2) {
            if (gameGrid[i - 1][gameCols - 1].getText().equals(type)
                    && gameGrid[i][gameCols - 1].getText().equals(type)
                    && gameGrid[i + 1][gameCols - 1].getText().equals(type)) {
                return player.getName();
            }
        }

        //rowcheck
        if (gameGrid[i][j - 1].getText().equals(type)
                && gameGrid[i][j].getText().equals(type)
                && gameGrid[i][j + 1].getText().equals(type)) {
                return player.getName();
        } //colcheck
        else if (gameGrid[i - 1][j].getText().equals(type)
                && gameGrid[i][j].getText().equals(type)
                && gameGrid[i + 1][j].getText().equals(type)) {
                return player.getName();
        } //right diagonalcheck
        else if (gameGrid[i - 1][j - 1].getText().equals(type)
                && gameGrid[i][j].getText().equals(type)
                && gameGrid[i + 1][j + 1].getText().equals(type)) {
                return player.getName();
        } else if (gameGrid[i - 1][j + 1].getText().equals(type)
                && gameGrid[i][j].getText().equals(type)
                && gameGrid[i + 1][j - 1].getText().equals(type)) {
                return player.getName();
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