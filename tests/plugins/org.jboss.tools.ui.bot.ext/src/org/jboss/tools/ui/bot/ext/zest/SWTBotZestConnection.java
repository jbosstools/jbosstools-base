package org.jboss.tools.ui.bot.ext.zest;

import org.eclipse.draw2d.geometry.Point;
import org.eclipse.draw2d.geometry.PointList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBot;
import org.eclipse.zest.core.widgets.Graph;
import org.eclipse.zest.core.widgets.GraphConnection;

/**
 * SWTBot zest connnection bot
 * 
 * @author jpeterka
 * 
 */
public class SWTBotZestConnection extends AbstractSWTBot<GraphConnection> {


	/**
	 * Default Constructor
	 * @param connection
	 */
	public SWTBotZestConnection(GraphConnection connection) {
		super(connection);
	}

	/**
	 * Provides click above Zest Connection
	 */
	@Override
	public AbstractSWTBot<GraphConnection> click() {
		UIThreadRunnable.syncExec(new VoidResult() {

			public void run() {
				// Get midpoint
				Graph graph = widget.getGraphModel();
				PointList pl = widget.getConnectionFigure().getPoints();
				Point midpoint = pl.getMidpoint();

				Event event = new Event();
				event.type = SWT.MouseMove;
				event.x = graph.toDisplay(midpoint.x, midpoint.y).x;
				event.y = graph.toDisplay(midpoint.x, midpoint.y).y;
				widget.getDisplay().post(event);

				event = new Event();
				event.type = SWT.MouseDown;
				event.button = 1;
				widget.getDisplay().post(event);

				event = new Event();
				event.type = SWT.MouseUp;
				event.button = 1;
				widget.getDisplay().post(event);
			}
		});
		return this;
	}

	/**
	 * Returns context menu of connection 
	 * @return
	 */
	public SWTBotZestContextMenu contextMenu() {
		return new SWTBotZestContextMenu(widget.getGraphModel());
	}
}
