/*******************************************************************************
  * Copyright (c) 2010 - 2015 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.usage.tracker;

public interface IFocusPoint {

	public abstract String getName();

	public abstract IFocusPoint setChild(IFocusPoint childFocusPoint);

	public abstract IFocusPoint getChild();

	public abstract String getURI();

	public abstract String getTitle();

}