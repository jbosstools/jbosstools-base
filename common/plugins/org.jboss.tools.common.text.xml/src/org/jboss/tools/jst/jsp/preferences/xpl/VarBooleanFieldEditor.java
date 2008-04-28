/*******************************************************************************
 * Copyright (c) 2005 IBM Corporation and others.
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
package org.jboss.tools.jst.jsp.preferences.xpl;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

/**
 * @author Tau
 *
 */
public class VarBooleanFieldEditor extends BooleanFieldEditor {

	
	/**
	 * 
	 */
	public VarBooleanFieldEditor() {
		super();
	}
	
	/**
	 * @param name
	 * @param labelText
	 * @param style
	 * @param parent
	 */
	public VarBooleanFieldEditor(String name, String labelText, int style, Composite parent) {
		super(name, labelText, style, parent);
	}

	/**
	 * @param name
	 * @param label
	 * @param parent
	 */
	public VarBooleanFieldEditor(String name, String label, Composite parent) {
		super(name, label, parent);
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.preference.BooleanFieldEditor#getChangeControl(org.eclipse.swt.widgets.Composite)
	 */
	public Button getButton(Composite parent) {
		return super.getChangeControl(parent);
	}
	
	
	public void offset(Composite parent, int numPixel) {
		GridData gridData= new GridData();
		gridData.horizontalIndent= numPixel;		
		super.getChangeControl(parent).setLayoutData(gridData);
	}
}
