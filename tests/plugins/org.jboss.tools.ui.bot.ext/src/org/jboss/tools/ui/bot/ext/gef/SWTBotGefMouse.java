/*******************************************************************************
 * Copyright (c) 2007-2009 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.ui.bot.ext.gef;

import org.apache.log4j.Logger;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swtbot.eclipse.gef.finder.widgets.SWTBotGefFigureCanvas;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.VoidResult;

/**
 * Mouse control for GEF figures
 * 
 * @author jpeterka
 * 
 */
public class SWTBotGefMouse {

	Logger log = Logger.getLogger(SWTBotGefMouse.class);
	SWTBotGefFigureCanvas canvas;
	SWTBot bot;
	final int SLEEP = 100;

	/**
	 * Constructor, requires bot and canvas
	 * 
	 * @param bot
	 * @param canvas
	 */
	public SWTBotGefMouse(final SWTBot bot, final SWTBotGefFigureCanvas canvas) {
		this.canvas = canvas;
		this.bot = bot;
	}

	private Canvas canvas() {
		return canvas.widget;
	}

	/**
	 * Performs mouse move to particular location
	 * 
	 * @param x
	 * @param y
	 */
	public void move(final int x, final int y) {
		UIThreadRunnable.syncExec(new VoidResult() {
			public void run() {
				Event event = createEvent(SWT.MouseMove, x, y);
				canvas().getDisplay().post(event);
			}
		});
		bot.sleep(SLEEP);
	}

	/**
	 * Moves to given figure
	 * 
	 * @param figure
	 */
	public void move(SWTBotGefFigure figure) {
		Point p = new Point();
		getDisplayPosition(p, figure);
		move(p.x, p.y);
	}

	/**
	 * Moves and click on given figure
	 * 
	 * @param figure
	 */
	public void moveAndClick(SWTBotGefFigure figure) {
		move(figure);
		click();
	}
	
	/**
	 *  Moves and left mouse click on given position
	 */
	public void moveAndClick(int x, int y) {
		move(x,y);
		click();
	}
	
	/**
	 * Calculates element position to display, call it from UI thread
	 */
	private void getDisplayPosition(final Point point,
			final SWTBotGefFigure figure) {
		UIThreadRunnable.syncExec(new VoidResult() {
			public void run() {
				int absX = canvas().toDisplay(figure.getFigure().getBounds().x,
						figure.getFigure().getBounds().y).x;
				int absY = canvas().toDisplay(figure.getFigure().getBounds().x,
						figure.getFigure().getBounds().y).y;

				log.info("Figure abs display position:" + absX + "," + absY);
				point.x = absX + (figure.getFigure().getBounds().width / 2);
				point.y = absY + (figure.getFigure().getBounds().height / 2);
			}
		});
	}

	/**
	 * Moves on position related to canvas position
	 */
	public void moveCanvas(int x, int y) {
		
		final Point p = new Point();
		UIThreadRunnable.syncExec(new VoidResult() {
				public void run() {
					p.x = canvas().toDisplay(0,0).x;
					p.y = canvas().toDisplay(0,0).y;

					log.info("Canvas position on display is :" + p.x + "," + p.y);
				}
			});
		
		move(p.x + x, p.y + y);
	}
	
	/**
	 * Left click on give position
	 */
	public void click() {
		UIThreadRunnable.syncExec(new VoidResult() {
			public void run() {
				Event event = createEvent(SWT.MouseDown, 1);
				canvas().getDisplay().post(event);
			}
		});
		bot.sleep(SLEEP);
		UIThreadRunnable.syncExec(new VoidResult() {
			public void run() {
				Event event = createEvent(SWT.MouseUp, 1);
				canvas().getDisplay().post(event);
			}
		});
		bot.sleep(SLEEP);
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
