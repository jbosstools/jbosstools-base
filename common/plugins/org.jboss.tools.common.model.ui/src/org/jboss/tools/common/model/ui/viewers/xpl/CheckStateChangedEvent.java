/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
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
package org.jboss.tools.common.model.ui.viewers.xpl;

import java.util.EventObject;

/**
 * @author au
 */

public class CheckStateChangedEvent extends EventObject {

	private Object element;
	private int state;

	public CheckStateChangedEvent(ICheckable source, Object element, int state) {
		super(source);
		this.element = element;
		this.state = state;
	}

	public ICheckable getCheckable() {
		return (ICheckable) source;
	}

	public int getState() {
		return state;
	}

	public Object getElement() {
		return element;
	}
}
