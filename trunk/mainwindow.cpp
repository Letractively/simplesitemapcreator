/*
Copyright (c) 2013, Matthew Hipkin <http://www.matthewhipkin.co.uk>
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other
      materials provided with the distribution.
    * Neither the name of the developer nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior
      written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "mainwindow.h"
#include "ui_mainwindow.h"

QString APPNAME("Simple Sitemap Creator");
int CURRVER = 20131024;
QString APPVER("0.2");

static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

char* httpGet(char *url) {
    CURL *curl;
    CURLcode res;
    char *response;
    curl = curl_easy_init();
    if(curl) {      
      curl_easy_setopt(curl, CURLOPT_URL, url);
      curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
      curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
      //  http.UserAgent := 'Mozilla/4.0 (compatible; Simple Sitemap Creator '+APPVER+'; ' + OS + '; '+IntToStr(CURRVER)+'; +http://www.matthewhipkin.co.uk/apps/simplesitemapcreator/)'
      //curl_easy_setopt(curl, CURLOPT_USERAGENT, useragent);
      res = curl_easy_perform(curl);
      curl_easy_cleanup(curl);
    }
    return response;
}

void addLink(char *link, char *title, char *ref, char *header) {
    int x;

}

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    QFont font("Monospace");
    font.setStyleHint(QFont::TypeWriter);
    ui->textHTML->setFont(font);
    ui->textXML->setFont(font);
    highlighter = new XmlHighlighter(ui->textHTML->document());
    highlighter = new XmlHighlighter(ui->textXML->document());
    this->setWindowTitle(APPNAME+" "+APPVER);
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::on_btnGo_clicked()
{
    // Clear out bits and bobs first
    linkCount = 0;
    links.empty();
    ui->textHTML->clear();
    ui->textXML->clear();
}

void MainWindow::on_btnCopy_clicked()
{    
    QClipboard *cb = QApplication::clipboard();
    if(ui->tabHTML->isVisible()) cb->setText(ui->textHTML->toPlainText());
    else cb->setText(ui->textXML->toPlainText());
}

void MainWindow::on_btnClear_clicked()
{
    ui->textHTML->clear();
    ui->textXML->clear();
}

void MainWindow::on_btnSave_clicked()
{
    QString ext;
    if(ui->tabHTML->isVisible()) ext = ".html";
    else ext = ".xml";
    QString filename = QFileDialog::getSaveFileName(this, "Save file", "", ext);
    QFile f(filename);
    if(f.open(QIODevice::WriteOnly)) {
        QTextStream outstream(&f);
        if(ui->tabHTML->isVisible()) outstream << ui->textHTML->document()->toPlainText();
        else outstream << ui->textXML->document()->toPlainText();
        f.close();
    }
}

void MainWindow::on_btnAbout_clicked()
{
    QString html;
    html = "<p><b style=\"font-size: 14pt\">"+APPNAME+"</b> "+APPVER+"<br>\n";
    html.append("&copy;2010-2013 Matthew Hipkin<br>\n");
    html.append("<a href=\"http://www.matthewhipkin.co.uk\" style=\"color: #FF0000\">http://www.matthewhipkin.co.uk</a></p>\n");
    html.append("<p>A simple tool for creating HTML sitemaps.</p>");
    html.append("<p>Follow me on twitter <a href=\"http://twitter.com/hippy2094\" style=\"color: #FF0000\">@hippy2094</a></p>");
    QMessageBox::about(this,"About "+APPNAME,html);

}
