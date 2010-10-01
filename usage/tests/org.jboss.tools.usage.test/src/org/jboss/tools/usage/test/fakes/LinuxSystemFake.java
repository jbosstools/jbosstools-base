/*******************************************************************************
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test.fakes;

/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
import java.io.IOException;

import org.jboss.tools.usage.googleanalytics.eclipse.LinuxSystem;

/**
  * @author Andre Dietisheim
  */
public class LinuxSystemFake extends LinuxSystem {

	private String releaseFilePath;
	
	private String releaseFileContent;

	public LinuxSystemFake(String releaseFilePath, String releaseFileContent) {
		super();
		this.releaseFilePath = releaseFilePath;
		this.releaseFileContent = releaseFileContent;
	}

	@Override
	protected boolean exists(String releaseFilePath) {
		return this.releaseFilePath.equals(releaseFilePath);
	}

	@Override
	protected String getDistroFileContent(String filePath) throws IOException {
		return releaseFileContent;
	}
}