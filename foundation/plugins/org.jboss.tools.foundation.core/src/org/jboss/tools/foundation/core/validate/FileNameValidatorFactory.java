/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.core.validate;

import org.jboss.tools.foundation.core.validate.impl.FileNameValidator;

/**
 * Factory that provides an implementation of the {@link IFileNameValidator}
 */
public class FileNameValidatorFactory {

    /**
     * Create an instance of the default validator implementation
     *
     * @return instance of {@link IFileNameValidator}
     */
    public IFileNameValidator createValidator() {
        return new FileNameValidator();
    }
}
