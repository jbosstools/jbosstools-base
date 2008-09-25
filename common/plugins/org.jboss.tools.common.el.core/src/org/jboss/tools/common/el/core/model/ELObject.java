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
package org.jboss.tools.common.el.core.model;

import java.util.List;

import org.jboss.tools.common.el.core.parser.LexicalToken;

/**
 * Super type for all objects in EL model.
 * @author V. Kabanovich
 *
 */
public interface ELObject {

	public ELObjectType getType();

	public ELModel getModel();

	public int getStartPosition();
	public int getEndPosition();
	public int getLength();

	public String getText();

	public ELObject getParent();

	public List<ELObject> getChildren();

	public LexicalToken getFirstToken();

	public LexicalToken getLastToken();

}
