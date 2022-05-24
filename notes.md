# Optimize `voiceChannelMembers` and similiar Operations

```py
# The Guild class keeps track of a list of voicestates:

def __init__(...):
    ...
    for obj in guild.get('voice_states', []):
      self._update_voice_state(obj, int(obj['channel_id']))
    ...

def _update_voice_state(self, data, channel_id):
    user_id = int(data['user_id'])
    channel = self.get_channel(channel_id)
    try:
        # check if we should remove the voice state from cache
        if channel is None:
            after = self._voice_states.pop(user_id)
        else:
            after = self._voice_states[user_id]

        before = copy.copy(after)
        after._update(data, channel)
    except KeyError:
        # if we're here then we're getting added into the cache
        after = VoiceState(data=data, channel=channel)
        before = VoiceState(data=data, channel=None)
        self._voice_states[user_id] = after

    member = self.get_member(user_id)
    if member is None:
        try:
            member = Member(data=data['member'], state=self._state, guild=self)
        except KeyError:
            member = None

    return member, before, after
```

# TODO
- ID open type family for DB purposes
