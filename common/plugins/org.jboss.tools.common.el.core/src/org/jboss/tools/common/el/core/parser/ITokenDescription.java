/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core.parser;

/**
 * 
 * @author V. Kabanovich
 *
 */
public interface ITokenDescription {

	/**
	 * 
	 * @return Unique type of this kind of token.
	 */
	public int getType();

	/**
	 * 
	 * @return Human readable name.
	 */
	public String getName();

	/**
	 * Determines if this token is relevant at this point
	 * @param tokenizer
	 * @param last
	 * @param offset
	 * @return
	 */
	public boolean isStart(Tokenizer tokenizer, int offset);

	/**
	 * 
	 * @param tokenizer
	 * @param last
	 * @param offset
	 * @return
	 */
	public boolean read(Tokenizer tokenizer, int offset);

}
