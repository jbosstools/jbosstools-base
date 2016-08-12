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
package org.jboss.tools.runtime.ui.internal.dialogs;

import java.io.File;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimeDetectionProblem;
import org.jboss.tools.runtime.ui.RuntimeSharedImages;

/**
 * @author snjeza
 * 
 */
public class RuntimeLabelProvider extends LabelProvider implements
		ITableLabelProvider {

	public Image getColumnImage(Object element, int columnIndex) {
		if (columnIndex == 3) {
			if (element instanceof RuntimeDefinition) {
				RuntimeDefinition definition = (RuntimeDefinition) element;
				RuntimeDetectionProblem[] s = definition.getProblems();
				int maxError = IStatus.OK;
				for( int i = 0; i < s.length; i++ ) {
					maxError = (maxError > s[i].getSeverity() ? maxError : s[i].getSeverity());
				}
				if( maxError == IStatus.WARNING) {
					return RuntimeSharedImages.getImage(RuntimeSharedImages.QUICKFIX_WARN_PATH);
				}
				if( maxError == IStatus.ERROR) {
					return RuntimeSharedImages.getImage(RuntimeSharedImages.QUICKFIX_ERR_PATH);
				}
			}			
		}
		return null;
	}

	public String getColumnText(Object element, int columnIndex) {
		if (element instanceof RuntimeDefinition) {
			RuntimeDefinition definition = (RuntimeDefinition) element;
			if (columnIndex == 0) {
				return definition.getName();
			}
			if (columnIndex == 1) {
				return definition.getType();
			}
			if (columnIndex == 2) {
				return definition.getVersion();
			}
			if (columnIndex == 4) {
				File location = definition.getLocation();
				if (location != null) {
					return definition.getLocation().getAbsolutePath();
				}
			}
			if (columnIndex == 3) {
				RuntimeDetectionProblem[] all = definition.getProblems();
				StringBuilder sb = new StringBuilder();
				int warnCount = 0;
				int errorCount = 0;
				for( int i = 0; i < all.length; i++ ) {
					if( all[i].getSeverity() == IStatus.WARNING) {
						warnCount += 1;
					} else if( all[i].getSeverity() == IStatus.ERROR) {
						errorCount += 1;
					}
				}
				if( errorCount > 0 ) {
					sb.append(errorCount);
					sb.append(" error");
					if( errorCount > 1 ) {
						sb.append("s");
					}
					if( warnCount > 0 ) {
						sb.append(", ");
					}
				}
				if( warnCount > 0 ) {
					sb.append(warnCount);
					sb.append(" warning");
					if( warnCount > 1 ) {
						sb.append("s");
					}
				}
				return sb.toString();
			}
		}
		return null;
	}
}
