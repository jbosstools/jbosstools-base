/*******************************************************************************
 * Copyright (c) 2007-2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.ui.bot.ext.gef;

import org.eclipse.draw2d.FigureCanvas;
import org.eclipse.gef.GraphicalViewer;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swtbot.eclipse.gef.finder.SWTGefBot;
import org.eclipse.swtbot.eclipse.gef.finder.widgets.SWTBotGefEditor;
import org.eclipse.swtbot.eclipse.gef.finder.widgets.SWTBotGefFigureCanvas;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.Result;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotText;

/**
 * Extended version of gef editor, it wraps SWTBotGefEditor and provides api
 * needed for gef tests which are hardly reached from standard gef editor
 * @author jpeterka
 *
 */
public class SWTBotGefEditorExt {
	SWTGefBot bot;
	SWTBotGefEditor editor;
	SWTBotGefMouse mouse;

	/**
	 * Default constructor
	 */
	public SWTBotGefEditorExt(String title) {
		this.bot = new SWTGefBot();
		this.editor = bot.gefEditor(title);
		this.mouse = new SWTBotGefMouse(editor.bot(),
				SWTBotGefFinder.findCanvas(editor));
	}


	/**
	 * Insert entity from tool bar on given position
	 */
	public void insertEntity(String title, int x, int y) {
		editor.activateTool(title);
		editor.click(x, y);
	}

	/**
	 * get label figure
	 */
	public SWTBotGefFigure labelFigure(String label) {
		SWTBotGefFigure rf = getRootFigure();
		SWTBotGefFigure lf = rf.labelFigure(label);
		return lf;
	}

	/**
	 * set text to label figure
	 */
	public void setLabelText(SWTBotGefFigure figure, String str) {
		editor.select(figure.getText());
		mouse.moveAndClick(figure);
		SWTBotGefFigureCanvas canvas = SWTBotGefFinder.findCanvas(editor);
		SWTBotText text = bot.text(0);
		canvas.typeText(text.widget, str);
	}

	/**
	 * return root figure
	 */
	public SWTBotGefFigure getRootFigure() {
		SWTBotGefViewerExt viewer = new SWTBotGefViewerExt(editor);
		SWTBotGefFigure rf = viewer.getRootFigure();
		return rf;
	}

	/**
	 * returns canvas bounds
	 */
	public Rectangle getCanvasBounds() {
		Rectangle ret = UIThreadRunnable.syncExec(new Result<Rectangle>() {
			@Override
			public Rectangle run() {
				GraphicalViewer viewer = SWTBotGefFinder.getViewer(editor);
				FigureCanvas fc = (FigureCanvas) viewer.getControl();
				Rectangle r = fc.getBounds();
				return r;
			}
		});
		return ret;
	}

	// ---- Wrappers
	/**
	 * saves editor
	 */
	public void save() {
		editor.save();
	}
}
