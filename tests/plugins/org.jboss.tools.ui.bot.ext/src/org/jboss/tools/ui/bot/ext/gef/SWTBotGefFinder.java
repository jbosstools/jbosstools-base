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

import org.eclipse.draw2d.FigureCanvas;
import org.eclipse.gef.GraphicalViewer;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swtbot.eclipse.gef.finder.widgets.SWTBotGefEditor;
import org.eclipse.swtbot.eclipse.gef.finder.widgets.SWTBotGefFigureCanvas;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.Result;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;

/**
 * SWTBotGefProvider provides some often conversion tasks for getting
 * 
 * @author jpeterka
 * 
 */
public class SWTBotGefFinder {

	/**
	 * Returns Canvas from given GEF editor
	 * 
	 * @param editor
	 * @return
	 */
	public static SWTBotGefFigureCanvas findCanvas(final SWTBotGefEditor editor) {

		SWTBotGefFigureCanvas canvas = null;

		canvas = UIThreadRunnable.syncExec(new Result<SWTBotGefFigureCanvas>() {
			public SWTBotGefFigureCanvas run() {
				final IEditorPart ep = editor.getReference().getEditor(true);
				GraphicalViewer graphicalViewer = (GraphicalViewer) ep
						.getAdapter(GraphicalViewer.class);
				final Control control = graphicalViewer.getControl();
				if (control instanceof FigureCanvas) {
					return new SWTBotGefFigureCanvas((FigureCanvas) control);
				}
				return null;
			}
		});

		if (canvas == null) {
			throw new WidgetNotFoundException(
					"Unable to get Canvas from editor");
		}
		return canvas;
	}
	
	/**
	 * Returns graphical viewer from editor
	 */
	public static GraphicalViewer getViewer(final SWTBotGefEditor editor) 
	{
		GraphicalViewer graphicalViewer = UIThreadRunnable.syncExec(new Result<GraphicalViewer>() {
			public GraphicalViewer run() {
				IEditorReference partReference = editor.getReference();
				final IEditorPart editor = partReference.getEditor(true);
				return (GraphicalViewer) editor.getAdapter(GraphicalViewer.class);
			}
		});
				
		return graphicalViewer;
	}

}
