//// Workspace manager OTP actor for managing workspace state.
////
//// This module is a placeholder for future workspace management functionality.

import types.{type WorkspaceId}

/// Message type for workspace manager actor.
pub type WorkspaceManagerMessage {
  GetWorkspace(id: WorkspaceId)
}

/// Error type for workspace manager initialization.
pub type WorkspaceManagerError {
  InitializationFailed
}
