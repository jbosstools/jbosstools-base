/*******************************************************************************
  * Copyright (c) 2009 - 2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.zip;

import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class DefaultZipEntryVisitor implements IZipEntryVisitor {

	public void visiteDirectoryEntry(ZipFile zipFIle, ZipEntry dir) throws IOException {
	}

	public void visiteFileEntry(ZipFile zipFile, ZipEntry file) throws IOException {
	}
}
