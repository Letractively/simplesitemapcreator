#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QMessageBox>
#include <QClipboard>
#include <QFileDialog>
#include <QTextStream>
#include <QList>
#include <libxml2/libxml/HTMLparser.h>
#include <curl/curl.h>
#include "xmlhighlighter.h"

struct linkItem {
    QString title;
    QString link;
    QString referrer;
    QString modtime;
    bool parsed;
};

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT
    
public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();
    
private slots:
    void on_btnGo_clicked();
    void on_btnCopy_clicked();
    void on_btnClear_clicked();
    void on_btnSave_clicked();
    void on_btnAbout_clicked();

private:
    Ui::MainWindow *ui;
    XmlHighlighter *highlighter;
    int linkCount;
    QList<linkItem> links;
};

#endif // MAINWINDOW_H
