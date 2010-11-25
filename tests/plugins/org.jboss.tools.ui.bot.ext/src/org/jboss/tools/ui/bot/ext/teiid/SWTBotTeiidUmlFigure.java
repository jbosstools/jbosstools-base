package org.jboss.tools.ui.bot.ext.teiid;

import org.eclipse.draw2d.FigureCanvas;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.VoidResult;

import com.metamatrix.modeler.diagram.ui.notation.uml.figure.UmlClassifierFigure;

public class SWTBotTeiidUmlFigure extends SWTBotTeiidFigure{

	private UmlClassifierFigure fig;
	
	public SWTBotTeiidUmlFigure(UmlClassifierFigure fig, FigureCanvas c) {
		super(c);
		this.fig = fig;
	}

	@Override
	public void doubleClick() {
		/*
		System.err.println("SWTBotTeiidUmlFigure:" + fig.getLabelFigure().getText());
		System.err.println("SWTBotTeiidUmlFigure.LabelBounds:" + fig.getLabelFigure().getBounds());
		System.err.println("SWTBotTeiidUmlFigure.nameBounds:" + fig.getNameFigure().getBounds());
		
		
		doubleClickXY(fig.getLabelFigure().getBounds().getCenter().x, fig.getLabelFigure().getBounds().getCenter().y);
		//doubleClickXY(fig.getNameFigure().getBounds().getCenter().x, fig.getNameFigure().getBounds().getCenter().y);
		*/
		/*
		UIThreadRunnable.syncExec(new VoidResult() {
			
			@Override
			public void run() {
				//widget.forceFocus();
				
				
				// Move mouse
				Event event = new Event();
				event.type = SWT.MouseMove;
				event.x = widget.toDisplay(fig.getLabelFigure().getBounds().getCenter().x, fig.getLabelFigure().getBounds().getCenter().y).x;
				event.y = widget.toDisplay(fig.getLabelFigure().getBounds().getCenter().x, fig.getLabelFigure().getBounds().getCenter().y).y;
				widget.getDisplay().post(event);
				
				// Mouse down
				event = new Event();
				event.type = SWT.MouseDown;
				event.button = 1;
				widget.getDisplay().post(event);
				// Mouse Up
				event = new Event();
				event.type = SWT.MouseUp;
				event.button = 1;
				widget.getDisplay().post(event);
				// Mouse down
				event = new Event();
				event.type = SWT.MouseDown;
				event.button = 1;
				widget.getDisplay().post(event);
				// Mouse Up
				event = new Event();
				event.type = SWT.MouseUp;
				event.button = 1;
				widget.getDisplay().post(event);
			
			}
		});
		*/
	}
	
}
