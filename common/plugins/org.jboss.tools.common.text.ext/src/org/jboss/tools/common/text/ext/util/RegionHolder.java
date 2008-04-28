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
package org.jboss.tools.common.text.ext.util;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.IRegion;

/**
 * @author Jeremy
 *
 */
public class RegionHolder {
	public IFile file;
	public IRegion region;
	
	public RegionHolder (IFile file, IRegion region) {
		this.file = file;
		this.region = region;
	}
	
	public RegionHolder (IRegion region) {
		this(null, region);
	}
}
