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

import org.eclipse.gef.GraphicalEditPart;
import org.eclipse.gef.GraphicalViewer;
import org.eclipse.gef.RootEditPart;
import org.eclipse.swtbot.eclipse.gef.finder.widgets.SWTBotGefEditor;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.Result;

/**
 * Provides SWTBot Gef support related to Gef Viewer
 * @author jpeterka
 *
 */
public class SWTBotGefViewerExt {

	GraphicalViewer graphicalViewer;

	public SWTBotGefViewerExt(SWTBotGefEditor editor) {
		graphicalViewer = SWTBotGefFinder.getViewer(editor);
	}

	/**
	 * Returns root figure
	 * @return
	 */
	public SWTBotGefFigure getRootFigure() {
		SWTBotGefFigure ret = UIThreadRunnable.syncExec(new Result<SWTBotGefFigure>() {

			@Override
			public SWTBotGefFigure run() {
				RootEditPart root = graphicalViewer.getRootEditPart().getRoot();
				if (root instanceof GraphicalEditPart) { 
					GraphicalEditPart gep = (GraphicalEditPart)root;
					SWTBotGefFigure figure = new SWTBotGefFigure(gep.getFigure());
					return figure;
				}
				else {
					throw new WidgetNotFoundException("Can't get root figure");
				}
			}
		});
		return ret;
	}

}
