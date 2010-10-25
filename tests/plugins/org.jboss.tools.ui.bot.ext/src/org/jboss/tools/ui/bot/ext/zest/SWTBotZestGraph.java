package org.jboss.tools.ui.bot.ext.zest;

import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.draw2d.EventDispatcher;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.draw2d.geometry.PointList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.eclipse.swtbot.swt.finder.results.WidgetResult;
import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBot;
import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBotControl;
import org.eclipse.zest.core.widgets.Graph;
import org.eclipse.zest.core.widgets.GraphConnection;
import org.eclipse.zest.core.widgets.GraphNode;

/**
 * Zest Graph Model bot
 * 
 * @author jpeterka
 * 
 */
public class SWTBotZestGraph extends AbstractSWTBotControl<Graph> {

	Logger log = Logger.getLogger(SWTBotZestGraph.class);

	protected EventDispatcher eventDispatcher;

	public SWTBotZestGraph(Graph g) {
		super(g);
		eventDispatcher = g.getLightweightSystem().getRootFigure()
				.internalGetEventDispatcher();
	}

	/**
	 * Performs click above Bot zest Graph
	 */
	@Override
	protected AbstractSWTBot<Graph> click() {
		UIThreadRunnable.syncExec(new VoidResult() {

			public void run() {
				// Move mouse
				Event event = new Event();
				event.type = SWT.MouseMove;
				event.x = widget.toDisplay(widget.getLocation().x, widget
						.getLocation().y).x;
				event.y = widget.toDisplay(widget.getLocation().x, widget
						.getLocation().y).y;
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
		return this;
	}

	/**
	 * 
	 * @param nodeText
	 * @return
	 */
	public SWTBotZestNode node(final String nodeText) {

		GraphNode node = UIThreadRunnable
				.syncExec(new WidgetResult<GraphNode>() {

					@SuppressWarnings("unchecked")
					public GraphNode run() {
						List<GraphNode> nodes = widget.getNodes();
						for (GraphNode n : nodes) {

							System.out.println(n.getText());
							if (n.getText().equals(nodeText))
								return n;
						}
						return null;
					}
				});
		return new SWTBotZestNode(node);
	}

	/***
	 * Returns Graph connection given by index
	 * 
	 * @param index
	 * @return
	 */
	public SWTBotZestConnection connection(final int index) {
		GraphConnection connection = UIThreadRunnable
				.syncExec(new WidgetResult<GraphConnection>() {

					@SuppressWarnings("unchecked")
					public GraphConnection run() {
						List<GraphConnection> connections = widget
								.getConnections();
						if (connections.size() > index - 1) {
							return connections.get(index);
						}
						throw new WidgetNotFoundException(
								"Connection widget not found");
					}
				});
		return new SWTBotZestConnection(connection);
	}

	/**
	 * Returns SWTBotZestConnection given by two Zest Nodes
	 * @param node1
	 * @param node2
	 * @return
	 */
	public SWTBotZestConnection connection(final SWTBotZestNode node1,
			final SWTBotZestNode node2) {
		GraphConnection connection = UIThreadRunnable
				.syncExec(new WidgetResult<GraphConnection>() {

					@SuppressWarnings("unchecked")
					public GraphConnection run() {
						List<GraphConnection> connections = widget
								.getConnections();

						for (GraphConnection c : connections) {
							log.info("Node1" + node1.widget.getText());
							log.info("Node2" + node2.widget.getText());
							log.info("Source" + c.getSource().getText());
							log.info("Destination"
									+ c.getDestination().getText());
							if ((c.getSource().getText().equals(node1.widget
									.getText()))
									&& (c.getDestination().getText()
											.equals(node2.widget.getText())))
								return c;
						}

						throw new WidgetNotFoundException(
								"Connection widget not found");
					}
				});
		return new SWTBotZestConnection(connection);
	}

	/**
	 * Debug method, logs graph related informations
	 */
	public void debugGraph() {
		UIThreadRunnable.syncExec(new VoidResult() {
			@SuppressWarnings("unchecked")
			public void run() {
				// Graph Info
				log
						.info("********************************************************************************");

				Graph g = widget;
				log.info("Graph Nodes:" + g.getNodes().size());
				log.info("Graph Connections" + g.getConnections().size());

				Point leftUpper = new Point(g.getBounds().x, g.getBounds().y);
				Point rightLower = new Point(g.getBounds().x
						+ g.getBounds().width, g.getBounds().y
						+ g.getBounds().height);
				int gXtoDisp = widget.toDisplay(leftUpper.x, leftUpper.y).x;
				int gYtoDisp = widget.toDisplay(leftUpper.x, leftUpper.y).y;

				log.info("Graph position:[" + gXtoDisp + "," + gYtoDisp
						+ "] -> [" + rightLower.x + "," + rightLower.y + "]");
				log
						.info("********************************************************************************");

				// List Connections
				List<GraphConnection> connections = widget.getConnections();
				for (int i = 0; i < connections.size(); i++) {
					GraphConnection c = connections.get(i);

					PointList pl = c.getConnectionFigure().getPoints();
					Point midpoint = pl.getMidpoint();
					int mXtoDisp = widget.toDisplay(midpoint.x, midpoint.y).x;
					int mYtoDisp = widget.toDisplay(midpoint.x, midpoint.y).y;

					log.info("Connection:" + i + "[" + mXtoDisp + ","
							+ mYtoDisp + "]" + ",Text:" + c.getText()
							+ ",Tooltip:" + c.getTooltip());

				}
				// List Nodes
				List<GraphNode> nodes = widget.getNodes();
				for (int i = 0; i < nodes.size(); i++) {
					GraphNode n = nodes.get(i);
					int nXtoDisp = widget.toDisplay(n.getLocation().x, n
							.getLocation().y).x;
					int nYtoDisp = widget.toDisplay(n.getLocation().x, n
							.getLocation().y).y;

					log.info("Node:" + i + "[" + nXtoDisp + "," + nYtoDisp
							+ "],Text:" + n.getText() + ",Tooltip:"
							+ n.getTooltip());
				}
				log
						.info("********************************************************************************");
			}
		});
	}

	/**
	 * Returns Context menu
	 * @return
	 */
	public SWTBotZestContextMenu contextMenu() {
		return new SWTBotZestContextMenu(widget);
	}
}
