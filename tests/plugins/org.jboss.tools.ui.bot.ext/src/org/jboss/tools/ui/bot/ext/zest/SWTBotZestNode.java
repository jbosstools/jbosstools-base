package org.jboss.tools.ui.bot.ext.zest;

import org.eclipse.draw2d.geometry.Point;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBot;
import org.eclipse.zest.core.widgets.Graph;
import org.eclipse.zest.core.widgets.GraphNode;

/**
 * Provides SWTBot feature for Zest Node
 * 
 * @author jpeterka
 * 
 */
public class SWTBotZestNode extends AbstractSWTBot<GraphNode> {

	public SWTBotZestNode(GraphNode node) {
		super(node);
	}

	/**
	 * Calculates element position to display, call it from UI thread
	 */
	private void getDisplayPosition(Point point) {

		Graph graph = widget.getGraphModel();

		int absX = graph.toDisplay(widget.getLocation().x,
				widget.getLocation().y).x;
		int absY = graph.toDisplay(widget.getLocation().x,
				widget.getLocation().y).y;

		log.info("Node display position:" + absX + "," + absY);
		point.x = absX;
		point.y = absY;
	}

	/**
	 * Provides click on Zest Node
	 */
	@Override
	public AbstractSWTBot<GraphNode> click() {
		UIThreadRunnable.syncExec(new VoidResult() {

			public void run() {

				Point p = new Point();
				getDisplayPosition(p);

				Event event = createEvent(SWT.MouseMove, p.x, p.y);
				widget.getDisplay().post(event);

				event = createEvent(SWT.MouseDown, 1);
				widget.getDisplay().post(event);

				event = createEvent(SWT.MouseUp, 1);
				widget.getDisplay().post(event);
			}
		});
		return this;
	}

	/**
	 * Provides dragTo method for Node. Node will be moved relatively according
	 * to x,y
	 * 
	 * @param x
	 *            relative points how element will be moved in x axis
	 * @param y
	 *            relative points how element will be moved in y axis
	 * @throws InterruptedException
	 */
	public void dragTo(int x, int y) throws InterruptedException {

		final Point point = new Point();

		UIThreadRunnable.syncExec(new VoidResult() {
			public void run() {
				getDisplayPosition(point);
			}
		});

		log.info("Drag from:" + point.x + "," + point.y);
		log.info("Drop to:" + (point.x + x) + "," + (point.y + y));
		mouseDrag(point.x, point.y, point.x + x, point.y + y);
	}

	/**
	 * Returns SWTBotZestContextMenu, contextMenu
	 * 
	 * @return
	 */
	public SWTBotZestContextMenu contextMenu() {
		return new SWTBotZestContextMenu(widget.getGraphModel());
	}

	/**
	 * Provides drag and drop of node in absolute positions
	 * 
	 * @param fromXPosition
	 * @param fromYPosition
	 * @param toXPosition
	 * @param toYPosition
	 * @throws InterruptedException
	 */
	public void mouseDrag(final int fromXPosition, final int fromYPosition,
			final int toXPosition, final int toYPosition)
			throws InterruptedException {
		UIThreadRunnable.syncExec(new VoidResult() {
			public void run() {
				Event event = createEvent(SWT.MouseMove, fromXPosition,
						fromYPosition);
				widget.getDisplay().post(event);
			}
		});
		Thread.sleep(100);
		UIThreadRunnable.syncExec(new VoidResult() {
			public void run() {
				Event event = createEvent(SWT.MouseDown, 1);
				widget.getDisplay().post(event);
			}
		});
		Thread.sleep(100);
		UIThreadRunnable.syncExec(new VoidResult() {
			public void run() {
				Event event = createEvent(SWT.MouseMove, toXPosition,
						toYPosition);
				widget.getDisplay().post(event);
			}
		});
		Thread.sleep(100);
		UIThreadRunnable.syncExec(new VoidResult() {
			public void run() {
				Event event = createEvent(SWT.MouseUp, 1);
				widget.getDisplay().post(event);
			}
		});
	}

	private Event createEvent(int type, int x, int y) {
		Event event = new Event();
		event.type = type;
		event.x = x;
		event.y = y;
		return event;
	}

	private Event createEvent(int type, int button) {
		Event event = new Event();
		event.type = type;
		event.button = button;
		return event;
	}
}
