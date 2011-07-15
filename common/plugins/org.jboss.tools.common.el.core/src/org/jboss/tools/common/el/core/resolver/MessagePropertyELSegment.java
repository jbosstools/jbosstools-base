/******************************************************************************* 
 * Copyright (c) 2010 Red Hat, Inc. 
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
import org.jboss.tools.common.text.ITextSourceReference;

/**
 * Describes a segment of EL operand which is a Message Bundle Property. 
 * @author Daniel Azarov
 */
public interface MessagePropertyELSegment extends ELSegment {
	/**
	 * @return qualified name of property file.
	 */
	String getBaseName();

	/**
	 * @return resource of Message Bundle.
	 */
	IFile getMessageBundleResource();

	/**
	 * @return true if the segment presents message bundle. In this case isProperty() always returns false; 
	 */
	boolean isBundle();

	/**
	 * @return true if the segment presents message property.
	 */
	boolean isProperty();

	/**
	 * @return source reference of message property.
	 */
	ITextSourceReference getMessagePropertySourceReference();
}