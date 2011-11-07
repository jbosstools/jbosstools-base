/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.base.test.contentassist;

import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.texteditor.ITextEditor;

public class JavaContentAssistantTestCase extends AbstractContentAssistantTestCase {
	
	protected void obtainTextEditor(IEditorPart editorPart) {
		if(editorPart instanceof JavaEditor) {
			textEditor = (ITextEditor)editorPart;
		}
	}

	@Override
	protected ISourceViewer getTextViewer() {
		return ((JavaEditor)textEditor).getViewer();
	}

}
