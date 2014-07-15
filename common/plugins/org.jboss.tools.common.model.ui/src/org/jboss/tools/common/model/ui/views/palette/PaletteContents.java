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
package org.jboss.tools.common.model.ui.views.palette;

import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.jboss.tools.common.model.ui.editor.EditorDescriptor;
import org.jboss.tools.common.util.FileUtil;

public class PaletteContents {
	private IEditorPart editorPart;
	private String[] natureTypes;
	private String[] editorTypes;

	public static String TYPE_MOBILE = "mobile"; //$NON-NLS-1$
	public static String TYPE_JSF = "jsf"; //$NON-NLS-1$

	public PaletteContents(IEditorPart editorPart) {
		this.editorPart = editorPart;
		if (editorPart == null) {
			emptyInit();
			return;
		}
		
		EditorDescriptor editorDescriptor = (EditorDescriptor)editorPart.getAdapter(EditorDescriptor.class);
		if (editorDescriptor != null)
			editorTypes = editorDescriptor.getEditorTypes();
		else
			editorTypes = new String[0];

		natureTypes = computeNatureTypes(); 
	}

	public boolean update() {
		if(editorPart == null) return false;
		String[] ns = computeNatureTypes();
		if(changed(ns)) {
			natureTypes = ns;
			return true;
		}		
		return false;
	}

	boolean changed(String[] ns) {
		return (natureTypes.length != ns.length || !natureTypes[0].equals(ns[0])); 
	}

	private String[] computeNatureTypes() {
		String[] result = new String[0];
		if(editorPart != null) {
			IEditorInput input = editorPart.getEditorInput();
			if (input instanceof IFileEditorInput) {
				IFile file = ((IFileEditorInput)input).getFile();
				if(file != null) {
					String doctype = FileUtil.getDoctype(FileUtil.getContentFromEditorOrFile(file));
					if("html".equalsIgnoreCase(doctype) 
						|| (doctype == null && file.getName().endsWith(".html"))
							) {
						result = new String[]{TYPE_MOBILE}; //$NON-NLS-1$
					} else {
						result = new String[]{TYPE_JSF}; //$NON-NLS-1$
					}
				}
			}
		}		
		return result;
	}

	private void emptyInit() {
		natureTypes = new String[0];
		editorTypes = new String[0];
	}

	public boolean empty() {
		return natureTypes.length <= 0 && editorTypes.length <= 0; 
	}
	
	public String[] getNatureTypes() {
		return natureTypes;
	}
	
	public String[] getEditorTypes() {
		return editorTypes;
	}

}
