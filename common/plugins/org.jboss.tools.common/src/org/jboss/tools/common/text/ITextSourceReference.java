 /*******************************************************************************
  * Copyright (c) 2007 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.text;

/**
 * An interface of seam tools model object that has text source.
 * @author Alexey Kazakov
 */
public interface ITextSourceReference {

	/**
	 * @return start position of element in text
	 */
	public int getStartPosition();

	/**
	 * @return number of characters of element in text
	 */
	public int getLength();
}