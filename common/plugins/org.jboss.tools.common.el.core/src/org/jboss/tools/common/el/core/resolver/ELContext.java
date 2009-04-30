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
package org.jboss.tools.common.el.core.resolver;

import org.eclipse.core.resources.IFile;

/**
 * @author Alexey Kazakov
 */
public interface ELContext {

	/**
	 * Returns Resource of the page
	 * @return
	 */
	IFile getResource();

	/**
	 * Returns "var" attributes which are available in particular offset.
	 * @param offset
	 * @return
	 */
	Var[] getVars(int offset);
}