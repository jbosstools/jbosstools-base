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

	/** release file paths on the faked system */
	private ReleaseFile[] releaseFiles;
	
	public LinuxSystemFake(ReleaseFile... releaseFiles) {
		this.releaseFiles = releaseFiles;
	}

	@Override
	protected boolean exists(String releaseFilePath) {
		for (ReleaseFile releaseFile : releaseFiles) {
			if(releaseFile.getPath().equals(releaseFilePath)) {
				return true;
			}
		}
		return false;
	}

	@Override
	protected String getDistroFileContent(String filePath) throws IOException {
		for (ReleaseFile releaseFile : releaseFiles) {
			if (releaseFile.getPath().equals(filePath)) {
				return releaseFile.getContent();
			}
		}
		return null;
	}

	public static class ReleaseFile {
		private String path;
		private String content;

		public ReleaseFile(String path, String content) {
			this.path = path;
			this.content = content;
		}

		public String getPath() {
			return path;
		}

		public String getContent() {
			return content;
		}
	}


}