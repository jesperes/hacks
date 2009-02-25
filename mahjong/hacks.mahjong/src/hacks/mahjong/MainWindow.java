package hacks.mahjong;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

public class MainWindow {
	private Display display;
	private Shell shell;
	private Image image;

	public MainWindow() {
		display = new Display();
		shell = new Shell(display);
		image = new Image(display, "icon16.png");

		shell.setText("Mahjong");
		shell.setImage(image);
		shell.setMinimumSize(400, 300);

		createControls();
	}

	public void createControls() {
		FillLayout fillLayout = new FillLayout();
		fillLayout.type = SWT.VERTICAL;
		fillLayout.marginHeight = 8;
		fillLayout.marginWidth = 8;

		shell.setLayout(fillLayout);

		Table table0 = new Table(shell, SWT.BORDER);
		table0.setLinesVisible(true);
		table0.setHeaderVisible(true);

		for (int i = 0; i < 4; i++) {
			TableColumn col = new TableColumn(table0, SWT.LEFT);
			col.setText(String.format("Player %d", i));
			col.setWidth(80);
		}

		Group group1 = new Group(shell, SWT.NONE);
		group1.setText("Hand");
	}

	public void run() {
		shell.pack();
		shell.open();

		while (!shell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
