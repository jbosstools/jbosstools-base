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
package org.jboss.tools.common.meta;

import java.util.Set;

/**
 * Provides ancestor-descendant dependencies of entities 
 * without loading entity details. 
 * 
 * @author glory
 */

public interface XParents {
	/**
	 * Returns true if entity1 is descendant of entity2.
	 * @param entity1
	 * @param entity2
	 * @return
	 */
	public boolean isDescendant(String entity1, String entity2);

	/**
	 * Returns set of names of entities which contain the parameter 
	 * entity whitin their children or grandchildren of any level. 
	 * @param entity
	 * @return
	 */
	public Set getAncestors(String entity);
}
