/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.el.core.resolver;

import org.eclipse.swt.graphics.Image;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public interface IOpenableReference {

	/**
	 * Opens editor and selects an object, defined by this reference.
	 * Returns true if editor is successfully opened.
	 * 
	 * @return true if editor is successfully opened
	 */
	public boolean open();

	/**
	 * 
	 * @return Text representation for option in the open-on menu.
	 */
	public String getLabel();

	/**
	 * 
	 * @return Image representation for option in the open-on menu.
	 */
	public Image getImage();

}
