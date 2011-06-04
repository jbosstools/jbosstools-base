/*************************************************************************************
 * Copyright (c) 2010-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui;

import java.io.File;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.runtime.core.model.ServerDefinition;

/**
 * @author snjeza
 * 
 */
public class RuntimeLabelProvider extends LabelProvider implements
		ITableLabelProvider {

	public Image getColumnImage(Object element, int columnIndex) {
		return null;
	}

	public String getColumnText(Object element, int columnIndex) {
		if (element instanceof ServerDefinition) {
			ServerDefinition definition = (ServerDefinition) element;
			if (columnIndex == 0) {
				return definition.getName();
			}
			if (columnIndex == 1) {
				return definition.getVersion();
			}
			if (columnIndex == 2) {
				return definition.getType();
			}
			if (columnIndex == 3) {
				File location = definition.getLocation();
				if (location != null) {
					return definition.getLocation().getAbsolutePath();
				}
			}
			if (columnIndex == 4) {
				return definition.getDescription();
			}
		}
		return null;
	}
}
