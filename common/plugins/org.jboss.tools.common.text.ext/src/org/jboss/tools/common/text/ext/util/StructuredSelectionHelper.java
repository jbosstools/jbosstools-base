/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.ext.util;

import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.wst.sse.core.internal.provisional.IndexedRegion;
import org.eclipse.wst.sse.ui.StructuredTextEditor;

import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.IEditorWrapper;
import org.jboss.tools.common.text.ext.IMultiPageEditor;

/**
 * @author Jeremy
 */
public class StructuredSelectionHelper {
	
	public static IndexedRegion getSourceElement(StructuredTextEditor editor, int offset) {
		try {
			return editor.getModel().getIndexedRegion(offset);
		} catch (Exception x) {
			return null;
		}
	}
	
	public static void setSelectionAndRevealInActiveEditor(IRegion region) {
		try {
			setSelectionAndReveal(null, region);
		} catch (Exception x) {
			ModelPlugin.log("Error in setting selection to active text editor", x);
		}
	}
	public static StructuredTextEditor getStructuredTextEditor (IEditorPart editorPart) {
		try {
			IEditorPart activeEditorPart = editorPart;
			if (activeEditorPart == null) {
				IWorkbenchPage workbenchPage = ExtensionsPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
				activeEditorPart = workbenchPage.getActiveEditor();
			}
			if (activeEditorPart instanceof IMultiPageEditor) {
				IMultiPageEditor mpEditor = (IMultiPageEditor)activeEditorPart;
				return (StructuredTextEditor)mpEditor.getSourceEditor();
			} else if (activeEditorPart instanceof IEditorWrapper) {
				IEditorWrapper editorPartWraper = (IEditorWrapper)activeEditorPart;
				return getStructuredTextEditor(editorPartWraper.getEditor());
			} else if (activeEditorPart instanceof StructuredTextEditor) {
				return (StructuredTextEditor)activeEditorPart;
				
			}
		} catch (Exception x) {
			ModelPlugin.log("Error in obtaining structured text editor", x);
		}
		return null;
	}
	
	public static void setSelectionAndReveal(IEditorPart editorPart, IRegion region) {
		try {
			StructuredTextEditor sourceEditor = getStructuredTextEditor(editorPart);
			if (sourceEditor == null) {
				if (editorPart instanceof ITextEditor) {
					((ITextEditor)editorPart).selectAndReveal(region.getOffset(), region.getLength());
				}
				return;
			}
			ISelectionProvider provider = sourceEditor.getSelectionProvider();
			provider.setSelection(new TextSelection(region.getOffset(), region.getLength()));
		} catch (Exception x) {
			ModelPlugin.log("Error in setting selection", x);
		}
	}

}
