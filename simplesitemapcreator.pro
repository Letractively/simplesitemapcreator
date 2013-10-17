#-------------------------------------------------
#
# Project created by QtCreator 2013-10-15T10:14:06
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = simplesitemapcreator
TEMPLATE = app


SOURCES += main.cpp\
        mainwindow.cpp\
        xmlhighlighter.cpp

HEADERS  += mainwindow.h\
        xmlhighlighter.h

FORMS    += mainwindow.ui

unix: CONFIG += link_pkgconfig
unix: PKGCONFIG += libcurl
unix: PKGCONFIG += libxml-2.0
