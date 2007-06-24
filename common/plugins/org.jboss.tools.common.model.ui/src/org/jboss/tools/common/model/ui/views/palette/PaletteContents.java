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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.EditorDescriptor;

public class PaletteContents {
	
	private String[] natureTypes;
	private String[] editorTypes;

	public PaletteContents(IEditorPart editorPart) {
		if (editorPart == null) {
			emptyInit();
			return;
		}
		
		EditorDescriptor editorDescriptor = (EditorDescriptor)editorPart.getAdapter(EditorDescriptor.class);
		if (editorDescriptor != null)
			editorTypes = editorDescriptor.getEditorTypes();
		else
			editorTypes = new String[0];

		IEditorInput input = editorPart.getEditorInput();
		if (!(input instanceof IFileEditorInput)) {
			natureTypes = new String[0];
			return;
		}
		IFile file = ((IFileEditorInput)input).getFile();
		if (file == null) {
			natureTypes = new String[0];
			return;
		}
		
		IProject project = file.getProject();
		List<String> natures = new ArrayList<String>();
		try {
			if (project.exists() && project.isOpen() && project.hasNature("org.jboss.tools.struts.strutsnature")) 
				natures.add("struts");
		} catch (CoreException e) {
			ModelUIPlugin.log(e);
		}
		try {
			if (project.exists() && project.isOpen() && project.hasNature("org.jboss.tools.jsf.jsfnature")) 
				natures.add("jsf");
		} catch (CoreException e) { 
			ModelUIPlugin.log(e);
		}
		if (natures.size() > 0) 
			natureTypes = natures.toArray(new String[natures.size()]); 
		else
			natureTypes = new String[0];
	}

	private void emptyInit() {
		natureTypes = new String[0];
		editorTypes = new String[0];
	}

	public boolean equalsContents(PaletteContents contents) {
		if (contents == null || contents.empty()) {
			return empty();
		}
		if (empty()) {
			return false;
		}
		if (!coincide(natureTypes, contents.getNatureTypes())) {
			return false;
		}
		if (!coincide(editorTypes, contents.getEditorTypes())) {
			return false;
		}
		return true;
	}

	public boolean contains(String[] natures, String[] editors) {
		return intersection(natureTypes, natures) && intersection(editorTypes, editors);
	}
	
	private boolean intersection(String[] environmentTypes, String[] paletteTypes) {
		if ((paletteTypes == null || paletteTypes.length <= 0)) {
			return true;
		}
		for (int i = 0; i < environmentTypes.length; i++) {
			for (int j = 0; j < paletteTypes.length; j++) {
				if (environmentTypes[i].equalsIgnoreCase(paletteTypes[j])) {
					return true;
				}
			}
		}
		return false;
	}
	
	private boolean coincide(String[] types1, String[] types2) {
		if (types1.length != types2.length) {
			return false;
		}
		for (int i = 0; i < types1.length; i++) {
			boolean found = false;
			for (int j = 0; j < types2.length; j++) {
				if (types1[i].equalsIgnoreCase(types2[j])) {
					found = true;
					break;
				}
			}
			if (!found) {
				return false;
			}
		}
		return true;
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
