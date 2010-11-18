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
package org.jboss.tools.common.ui.sidebyside.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.EditorSashContainer;
import org.eclipse.ui.internal.EditorStack;
import org.eclipse.ui.internal.ILayoutContainer;
import org.eclipse.ui.internal.LayoutPart;
import org.eclipse.ui.internal.PartPane;
import org.eclipse.ui.internal.PartSashContainer;
import org.eclipse.ui.internal.PartSite;
import org.eclipse.ui.internal.PartStack;
import org.eclipse.ui.internal.WorkbenchPage;
import org.eclipse.ui.internal.handlers.NewEditorHandler;

/**
 * Handler which split active editor vertically
 * @see https://jira.jboss.org/browse/JBIDE-6083
 * @author mareshkau
 */
public class SideBySideHandler extends NewEditorHandler {

	/**
	 * the command has been executed, so extract extract the needed information
	 * from the application context.
	 */
	public Object execute(ExecutionEvent event) throws ExecutionException {
			super.execute(event);
			splitEditorArea();
		return null;
	}

	private void splitEditorArea() {
		IWorkbenchPage workbenchPage = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage();
		IWorkbenchPart part = workbenchPage.getActivePart();
		PartPane partPane = ((PartSite) part.getSite()).getPane();
		LayoutPart layoutPart = partPane.getPart();

		// Get PartPane that correspond to the active editor
		PartPane currentEditorPartPane = ((PartSite) workbenchPage
				.getActiveEditor().getSite()).getPane();
		EditorSashContainer editorSashContainer = null;
		ILayoutContainer rootLayoutContainer = layoutPart.getContainer();
		if (rootLayoutContainer instanceof LayoutPart) {
			ILayoutContainer editorSashLayoutContainer = ((LayoutPart) rootLayoutContainer)
					.getContainer();
			if (editorSashLayoutContainer instanceof EditorSashContainer) {
				editorSashContainer = ((EditorSashContainer) editorSashLayoutContainer);
			}
		}
		/*
		 * Create a new part stack (i.e. a workbook) to home the
		 * currentEditorPartPane which hold the active editor
		 */
		PartStack newPart = createStack(editorSashContainer);

		editorSashContainer.stack(currentEditorPartPane, newPart);
		if (rootLayoutContainer instanceof LayoutPart) {
			ILayoutContainer cont = ((LayoutPart) rootLayoutContainer)
					.getContainer();
			if (cont instanceof PartSashContainer) {
				// "Split" the editor area by adding the new part
				((PartSashContainer) cont).add(newPart);
			}
		}
	}

	/**
	 * A method to create a part stack container (a new workbook)
	 * 
	 * @param editorSashContainer
	 *            the <code>EditorSashContainer</code> to set for the returned
	 *            <code>PartStack</code>
	 * @return a new part stack container
	 */
	private PartStack createStack(EditorSashContainer editorSashContainer) {
		WorkbenchPage workbenchPage = (WorkbenchPage) PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage();
		EditorStack newWorkbook = EditorStack.newEditorWorkbook(
				editorSashContainer, workbenchPage);
		return newWorkbook;
	}
}
