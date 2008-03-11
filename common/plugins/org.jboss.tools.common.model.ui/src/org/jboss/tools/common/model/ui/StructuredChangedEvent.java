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
package org.jboss.tools.common.model.ui;

import java.util.EventObject;

import org.eclipse.jface.viewers.IStructuredContentProvider;

public class StructuredChangedEvent extends EventObject {

	private IChange change;

	public StructuredChangedEvent(Object arg0) {
		super(arg0);
	}
	public StructuredChangedEvent(IStructuredContentProvider source, IChange change) {
		super(source);
		this.change = change;
	} 
	public IChange getChange() {
		return change;
	}
	public IStructuredContentProvider getStructuredContentProvider() {
		return (IStructuredContentProvider)super.getSource(); 
	}

}
