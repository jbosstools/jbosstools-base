/******************************************************************************* 
 * Copyright (c) 2015 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core.resolver;

/**
 * Used in XmlELCompletionProposalComputer.isELCAToBeShown() for IXmlContext.
 * 
 * @author Viacheslav Kabanovich
 *
 */
public interface ELResolverExtension {

	/**
	 * Returns true if this resolver may give EL resolutions for the resourse represented by the context parameter. 
	 * @param context
	 * @return
	 */
	boolean isRelevant(ELContext context);

}
