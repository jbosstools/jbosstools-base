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
package org.jboss.tools.common.model.impl;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;
import org.jboss.tools.common.model.XModelObject;

public class XModelObjectToResourceAdapter implements IAdapterFactory {

	public Object getAdapter(Object adaptableObject, Class adapterType) {
		if(adaptableObject instanceof XModelObject && adapterType == IResource.class) {
			XModelObject o = (XModelObject)adaptableObject;
			XModelObject f = ((XModelObjectImpl)o).getResourceAncestor();
			Object r = (f == null || f == this) ? null : f.getAdapter(IResource.class);
			return (r instanceof IResource) ? r : null;
		}
		return null;
	}

	public Class[] getAdapterList() {
		// TODO Auto-generated method stub
		return null;
	}

}
