package org.jboss.tools.runtime.ui;

import org.eclipse.swt.graphics.Image;
import org.jboss.tools.runtime.core.model.IRuntimeDetectionResolution;

public interface IRuntimeDetectionUIResolution extends IRuntimeDetectionResolution {
    /**
     * Returns optional additional information about the resolution.
     * The additional information will be presented to assist the user
     * in deciding if the selected proposal is the desired choice.
     *
     * @return the additional information or <code>null</code>
     */
    public String getDescription();

    /**
     * Returns the image to be displayed in the list of resolutions.
     * The image would typically be shown to the left of the display string.
     *
     * @return the image to be shown or <code>null</code> if no image is desired
     */
    public Image getImage();

}
