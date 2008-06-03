/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Exadel, Inc.
 *     Red Hat, Inc.
 *******************************************************************************/
package org.jboss.tools.common.text.xml.ui.xpl;

import org.eclipse.jface.text.ITextSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;

/**
 * Listener to be informed on text selection changes in an editor (post selection), including the corresponding AST.
 * The AST is shared and must not be modified.
 * Listeners can be registered in a <code>SelectionListenerWithASTManager</code>.
 */
public interface ISelectionListenerWithSM {
	
	/**
	 * Called when a selection has changed. The method is called in a post selection event in an background
	 * thread.
	 * @param part The editor part in which the selection change has occured.
	 * @param selection The new text selection
	 * @param astRoot The AST tree corresponding to the editor's input. This AST is shared and must
	 * not be modified.
	 */
	void selectionChanged(IEditorPart part, ITextSelection selection, IStructuredModel model);

}
