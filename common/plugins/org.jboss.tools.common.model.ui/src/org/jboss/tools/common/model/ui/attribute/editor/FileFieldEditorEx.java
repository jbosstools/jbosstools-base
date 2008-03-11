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
package org.jboss.tools.common.model.ui.attribute.editor;

import java.io.File;

import org.jboss.tools.common.model.ui.attribute.adapter.FileChooserAdapter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class FileFieldEditorEx extends DirectoryFieldEditorEx {	

	public FileFieldEditorEx() {}
		
	public FileFieldEditorEx(IWidgetSettings settings) {
		super(settings);
	}

	protected void init() {
		super.init();
	}

	public int getNumberOfControls() {
		return 2;
	}
	
	public Object callExternal(Shell shell) {
		FileDialog dialog = new FileDialog(shell, SWT.OPEN);
		String[] extensions = null;
		String[] filenames = null;
		if(propertyEditor != null && propertyEditor.getValue() != null) {
			Object v = propertyEditor.getValue();
			String fp = (v == null) ? null : v.toString();
			if(fp != null && fp.length() > 0) {
				File f = new File(fp);
				File p = f.getParentFile();
				if(p != null && p.exists()) {
					dialog.setFilterPath(p.getAbsolutePath());
				} else {  
					if (lastPath != null && new File(lastPath).exists()) dialog.setFilterPath(lastPath);
				}
				dialog.setFileName(f.getName());
			}
			FileChooserAdapter adapter = (FileChooserAdapter)propertyEditor.getAdapter(FileChooserAdapter.class);
			if(adapter != null) {
				extensions = adapter.getExtensions();
				filenames = adapter.getFileNames();
			}
		}
		if(propertyEditor == null || propertyEditor.getValue() == null || propertyEditor.getValue().toString().trim().length() == 0) {
			if (lastPath != null && new File(lastPath).exists()) dialog.setFilterPath(lastPath);
		}
		if (extensions != null)
			dialog.setFilterExtensions(extensions);
		if (filenames != null)
			dialog.setFilterNames(filenames);
			
		return dialog.open();
	}
	
	protected String changePressed() {
		Object v = callExternal(getShell());
		return (v == null) ? null : v.toString();
	}

}
