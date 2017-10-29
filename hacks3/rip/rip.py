#!/usr/bin/python

import pygtk
pygtk.require("2.0")
import gtk

import dvdtoc

class ContentsPage(gtk.VBox):
    def __init__(self):
        gtk.VBox.__init__(self)

        self.input_filename = None

        hbox = gtk.HBox()
        self.pack_start(hbox, gtk.FALSE, gtk.TRUE, 4)

        label = gtk.Label("Filename:")
        hbox.pack_start(label, gtk.FALSE, gtk.FALSE, 4)
        
        self.entry = gtk.Entry()
        hbox.pack_start(self.entry, gtk.TRUE, gtk.TRUE, 0)

        button = gtk.Button("Browse...")
        hbox.pack_start(button, gtk.FALSE, gtk.FALSE, 0)
        button.connect("clicked", self.on_browse)


        read_dvd_toc = gtk.Button("Read DVD TOC")
        self.pack_start(read_dvd_toc, gtk.TRUE, gtk.FALSE, 0)
        read_dvd_toc.connect("clicked", self.on_read_dvd_toc)

    def on_read_dvd_toc(self, widget):
        titles = dvdtoc.read_toc()
        

    def on_browse(self, widget):
        chooser = gtk.FileChooserDialog(title = "Select file",
                                        action = gtk.FILE_CHOOSER_ACTION_OPEN,
                                        buttons = (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                                                   gtk.STOCK_OPEN, gtk.RESPONSE_OK))

        if self.input_filename:
            chooser.set_filename(self.input_filename)
            
        result = chooser.run()
        if result == gtk.RESPONSE_OK:
            self.input_filename = chooser.get_filename()
            self.entry.set_text(self.input_filename)

        chooser.destroy()
        
                  
class ClippingPage(gtk.VBox):
    def __init__(self):
        gtk.VBox.__init__(self)
        self.add(gtk.Label("Clipping page"))


class MainNotebook(gtk.Notebook):
    def __init__(self):
        gtk.Notebook.__init__(self)
        self.set_tab_pos(gtk.POS_TOP)
        self.append_page(ContentsPage(), gtk.Label("Contents"))
        self.append_page(ClippingPage(), gtk.Label("Clipping"))

class MainWindow:
    def __init__(self):
        self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)

        self.window.connect("delete_event", self.delete_event)
        self.window.connect("destroy", self.destroy)

        self.main_vbox = gtk.VBox(gtk.FALSE, 1)
        self.window.add(self.main_vbox)

        self.mainmenu = self.create_main_menu()

        self.main_vbox.pack_start(self.mainmenu, gtk.FALSE, gtk.TRUE, 0)
        self.main_vbox.add(MainNotebook())

        self.window.set_size_request(300, 200)
        self.window.show_all()

    def create_main_menu(self):
        menu_items = (
            ( "/_File",         None,         None, 0, "<Branch>" ),
            ( "/File/_New",     "<control>N", None, 0, None ),
            ( "/File/_Open",    "<control>O", None, 0, None ),
            ( "/File/_Save",    "<control>S", None, 0, None ),
            ( "/File/Save _As", None,         None, 0, None ),
            ( "/File/sep1",     None,         None, 0, "<Separator>" ),
            ( "/File/Quit",     "<control>Q", gtk.main_quit, 0, None ),
            ( "/_Help",         None,         None, 0, "<LastBranch>" ),
            ( "/_Help/About",   None,         None, 0, None ),
            )

        accel_group = gtk.AccelGroup()
        item_factory = gtk.ItemFactory(gtk.MenuBar, "<main>", accel_group)
        item_factory.create_items(menu_items)
        self.window.add_accel_group(accel_group)

        self.item_factory = item_factory
        return item_factory.get_widget("<main>")
    
    def main(self):
        gtk.main()

    def delete_event(self, widget, event):
        return gtk.FALSE

    def destroy(self, widget):
        gtk.main_quit()
        
    
if __name__ == "__main__":
    mainwin = MainWindow()
    mainwin.main()
                
