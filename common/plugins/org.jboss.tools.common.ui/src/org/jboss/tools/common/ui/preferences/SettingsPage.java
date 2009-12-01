/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.ui.preferences;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.dialogs.PropertyPage;
import org.jboss.tools.common.ui.widget.editor.IFieldEditor;

/**
 * @author Alexey Kazakov
 */
public abstract class SettingsPage extends PropertyPage implements PropertyChangeListener {

	protected Map<String,IFieldEditor> editorRegistry = new HashMap<String,IFieldEditor>();

	protected void registerEditor(IFieldEditor editor, Composite parent) {
		editorRegistry.put(editor.getName(), editor);
		editor.doFillIntoGrid(parent);
		editor.addPropertyChangeListener(this);
	}

	public IFieldEditor getEditor(String editorName) {
		return editorRegistry.get(editorName);
	}

	public String getValue(String editorName) {
		return editorRegistry.get(editorName).getValue().toString();
	}

	/* (non-Javadoc)
	 * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
	 */
	public void propertyChange(PropertyChangeEvent evt) {
		validate();
	}

	protected abstract void validate();
}