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
package org.jboss.tools.common.model.ui.forms;

import java.util.Iterator;

public interface IFormCollection {
	// add
	public boolean addForm(IForm form);
	public void addForm(int index, IForm form);
	// remove
	public boolean removeForm(IForm form);
	public IForm removeForm(int index);
	// get
	public IForm get(int index);
	// contains
	public boolean contains(IForm form);
	// equals
	public boolean equals(IFormCollection formCollection);
	// size
	public int size();
	// interator
	public Iterator iterator();
	//clear
	public void clear();
}
