package games.tictactoeHost;

import javax.swing.*;

public class GameControl {

    private TicTacToe game;
    private int gameRows, gameCols;
    private String gameID;
    private JButton[][] buttonGrid;
    private TttPlayer x, o;

    public GameControl(String playerX, String playerO) {
        x = new TttPlayer(playerX, "X");
        o = new TttPlayer(playerO, "O");
        game = new TicTacToe(3, 3, this);
        gameRows = game.getRows();
        gameCols = game.getCols();
        buttonGrid = game.getGrid();
        game.run();
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

    public void winCheck(String type, TicTacToe g) {
        game = g;
        buttonGrid = game.getGrid();
        for (int i = 1; i < gameRows - 1; i++) {
            for (int j = 1; j < gameCols - 1; j++) {
                checkSurroundings(i, j, type);
            }
        }
    }

    private void checkSurroundings(int i, int j, String t) {
        TttPlayer player = (t.equals("X") ? x : o);
        String type = player.getType();
        if (i == 1) {
            if (buttonGrid[0][j - 1].getText().equals(type)
                    && buttonGrid[0][j].getText().equals(type)
                    && buttonGrid[0][j + 1].getText().equals(type)) {
                announceWinner(player);
            }
        }
        if (j == 1) {
            if (buttonGrid[i - 1][0].getText().equals(type)
                    && buttonGrid[i][0].getText().equals(type)
                    && buttonGrid[i + 1][0].getText().equals(type)) {
                announceWinner(player);
            }
        }
        if (i == gameRows - 2) {
            if (buttonGrid[gameRows - 1][j - 1].getText().equals(type)
                    && buttonGrid[gameRows - 1][j].getText().equals(type)
                    && buttonGrid[gameRows - 1][j + 1].getText().equals(type)) {
                announceWinner(player);
            }
        }
        if (j == gameCols - 2) {
            if (buttonGrid[i - 1][gameCols - 1].getText().equals(type)
                    && buttonGrid[i][gameCols - 1].getText().equals(type)
                    && buttonGrid[i + 1][gameCols - 1].getText().equals(type)) {
                announceWinner(player);
            }
        }

        //rowcheck
        if (buttonGrid[i][j - 1].getText().equals(type)
                && buttonGrid[i][j].getText().equals(type)
                && buttonGrid[i][j + 1].getText().equals(type)) {
            announceWinner(player);
        } //colcheck
        else if (buttonGrid[i - 1][j].getText().equals(type)
                && buttonGrid[i][j].getText().equals(type)
                && buttonGrid[i + 1][j].getText().equals(type)) {
            announceWinner(player);
        } //right diagonalcheck
        else if (buttonGrid[i - 1][j - 1].getText().equals(type)
                && buttonGrid[i][j].getText().equals(type)
                && buttonGrid[i + 1][j + 1].getText().equals(type)) {
            announceWinner(player);
        } else if (buttonGrid[i - 1][j + 1].getText().equals(type)
                && buttonGrid[i][j].getText().equals(type)
                && buttonGrid[i + 1][j - 1].getText().equals(type)) {
            announceWinner(player);
        } else {
            boardFull();
        }

    }

    private void announceWinner(TttPlayer player) {
        JOptionPane.showMessageDialog(game, player.getName() + " wins the game!");
        int ans = JOptionPane.showConfirmDialog(game, "Another round?", "Play again?", JOptionPane.YES_NO_OPTION);
        if (ans == 0) {
            game.reset();
        } else {
            game.dispose();
        }
    }

    public void boardFull() {
        for (int i = 0; i < gameRows; i++) {
            for (int j = 0; j < gameCols; j++) {
                if (!(buttonGrid[i][j].getText().equals(x.getType()) || buttonGrid[i][j].getText().equals(o.getType()))) {
                    return;
                }
                if (i == gameRows - 1 && j == gameCols - 1) {
                    announceWinner(new TttPlayer("Noone", "Noone"));
                }
            }
        }
    }

    public static void main(String[] args) {
        GameControl hoho = new GameControl("hej","san");
    }
}