/**********************************************************************
 * Copyright (c) 2005, 2006 Scapa Technologies Limited and others
 * 
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors: 
 * 		Scapa Technologies Limited - Initial API and implementation
 * 		Exadel, Inc.
 * 		Red Hat, Inc.
 **********************************************************************/
package org.jboss.tools.jst.jsp.preferences.xpl;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/**
 * 
 * @author eskimo
 *
 */
public class LabelFieldEditor extends FieldEditor {

	/*
	 * 
	 */
	private Label fLabel;

	/**
	 * Public constructor
	 * @param value
	 * @param parent
	 */
	public LabelFieldEditor(String value, Composite parent) {
		super("label", value, parent);
	}

	/**
	 * 
	 */
	protected void adjustForNumColumns(int columns) {
		((GridData) fLabel.getLayoutData()).horizontalSpan = columns;
	}

	/**
	 * 
	 */
	protected void doFillIntoGrid(Composite parent, int columns) {
		fLabel = getLabelControl(parent);
		
		GridData gd = new GridData();
		gd.horizontalSpan = columns;
		gd.horizontalAlignment = GridData.FILL;
		gd.grabExcessHorizontalSpace = false;
		gd.verticalAlignment = GridData.CENTER;
		gd.grabExcessVerticalSpace = false;
		
		fLabel.setLayoutData(gd);
	}

	/**
	 * 
	 */
	public int getNumberOfControls() {
		return 1;
	}

	/**
	 * 
	 */
	protected void doLoad() {
	}
	
	/**
	 * 
	 */
	protected void doLoadDefault() {
	}
	
	/**
	 * 
	 */
	protected void doStore() {
	}
}
