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
package org.jboss.tools.common.text;

/**
 * Represents a reference to XML node. For example there is
 * <class>
 * 		org.mycompany.Logger
 * </class>
 * then getValue() will return "org.mycompany.Logger"
 * and getStartPosition() will return the offset of the trimmed value (the index of "o")
 * and getLength() will return the length of "org.mycompany.Logger" string.
 * 
 * @author Alexey Kazakov
 */
public interface INodeReference extends ITextSourceReference {

	/**
	 * Returns the trimmed string representation of the value of the
	 * node. For example if this object is reference to
	 * <class>
	 *     org.mycompany.Logger
	 * </class>
	 * then this method will return "org.mycompany.Logger".
	 * 
	 * @return the trimmed string representation of the body of this XML
	 *         element.
	 */
	String getValue();
}